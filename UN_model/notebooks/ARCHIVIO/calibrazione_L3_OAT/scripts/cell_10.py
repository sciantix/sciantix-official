
# ============================================================
# MANUAL RUNNER, DIAGNOSTICS, AND PLOTS
# ============================================================

from pathlib import Path
import csv
import math
from dataclasses import replace
from typing import List, Dict

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

RUN_CACHE = {}

# Readable font sizes for individual plots placed side by side in Word.
# Word reduces each plot considerably when arranging four panels on one page.
MAIN_LABEL_FONTSIZE = 26
MAIN_TICK_FONTSIZE = 23
MAIN_LEGEND_FONTSIZE = 14
MAIN_INFOBOX_FONTSIZE = 14



# Experimental Xe diffusivity points digitized approximately from
# Schneider et al. (2026), Fig. 7; original experimental sources are
# Biddle et al., Kim et al., Melehan et al., and Oi et al.
#
# Embedded directly in the notebook so the overlay does NOT depend on an
# external CSV file. Values are stored as (T [K], log10(D_Xe [m^2/s])).
SCHNEIDER_FIG7_EXP_POINTS = {
    "Melehan et al.": [
        (1288, -19.3), (1296, -19.8),
        (1668, -17.5), (1668, -17.1),
        (1768, -17.0), (1768, -16.6),
        (1878, -16.4), (1885, -16.0),
    ],
    "Biddle et al.": [
        (1085, -22.6),
        (1280, -20.4), (1290, -20.0),
        (1490, -19.8),
        (1665, -18.6),
        (1860, -17.4),
    ],
    "Kim et al.": [
        (1478, -17.8), (1482, -18.2), (1485, -18.9), (1487, -19.4),
        (1575, -18.6),
        (1678, -18.4), (1682, -17.9),
        (1865, -18.2), (1868, -17.8),
    ],
    "Oi et al.": [
        (1530, -14.9),
        (1760, -14.2),
        (1860, -14.1), (1880, -13.9),
        (2080, -13.1), (2160, -13.2),
        (2340, -12.5), (2360, -12.4),
        (2500, -12.3),
    ],
}

# Deliberately use markers different from Schneider Fig. 7.
EXP_XE_MARKERS = {
    "Biddle et al.": "x",
    "Kim et al.": "+",
    "Melehan et al.": "P",
    "Oi et al.": "X",
}
EXP_XE_LABELS = {
    name: f"{name} (exp. Xe diffusivity)" for name in EXP_XE_MARKERS
}

def load_schneider_fig7_exp_points():
    records = []
    for experiment, points in SCHNEIDER_FIG7_EXP_POINTS.items():
        for T_K, log10_D in points:
            records.append({
                "experiment": experiment,
                "T_K": float(T_K),
                "log10_D_m2_s": float(log10_D),
                "D_m2_s": 10.0 ** float(log10_D),
            })
    return pd.DataFrame.from_records(records)

# ---- Rizk 2025 Fig. 9 digitized gas-partition reference curves ----
# Added in 7test_UN.
# Source file used to build these arrays:
# gas_partition_digitized_wide_25K.csv
#
# NOTE:
# - 8test separates grain-face bubbles and FGR in the model and in the Rizk overlay.
RIZK2025_GAS_PARTITION = {
    1.1: {
        "T_K": np.array([900.0, 925.0, 950.0, 975.0, 1000.0, 1025.0, 1050.0, 1075.0, 1100.0, 1125.0, 1150.0, 1175.0, 1200.0, 1225.0, 1250.0, 1275.0, 1300.0, 1325.0, 1350.0, 1375.0, 1400.0, 1425.0, 1450.0, 1475.0, 1500.0, 1525.0, 1550.0, 1575.0, 1600.0, 1625.0, 1650.0, 1675.0, 1700.0, 1725.0, 1750.0, 1775.0, 1800.0, 1825.0, 1850.0, 1875.0, 1900.0, 1925.0, 1950.0, 1975.0, 2000.0, 2025.0, 2050.0, 2075.0, 2100.0, 2125.0, 2150.0, 2175.0, 2200.0, 2225.0, 2250.0, 2275.0, 2300.0, 2325.0, 2350.0, 2375.0, 2400.0, 2425.0, 2450.0, 2475.0, 2500.0, 2525.0, 2550.0, 2575.0, 2600.0], dtype=float),
        "Matrix": np.array([5.9073, 5.888, 5.8687, 5.806, 5.7915, 5.7143, 5.7143, 5.6757, 5.6371, 5.6178, 5.5792, 5.5598, 5.5598, 5.5598, 5.5212, 5.5212, 5.4826, 5.4826, 5.444, 5.4054, 5.3282, 5.2239, 4.9807, 4.5907, 4.1313, 3.473, 2.8185, 2.1115, 1.7761, 1.1403, 0.695, 0.5985, 0.5405, 0.5019, 0.4633, 0.4247, 0.4247, 0.4247, 0.4247, 0.4247, 0.4247, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861], dtype=float),
        "Bulk bubbles": np.array([77.3745, 77.405, 77.471, 77.5386, 77.6448, 77.7262, 77.7799, 77.8407, 77.8764, 77.9416, 77.9549, 78.0444, 78.0695, 78.1467, 78.1367, 78.1588, 78.2239, 78.2722, 78.3205, 78.3301, 78.4098, 78.4556, 78.4942, 78.4942, 78.3956, 78.181, 77.7992, 76.5202, 75.0, 73.3809, 70.6178, 67.7992, 64.9421, 62.0463, 58.61, 55.184, 48.61, 42.21, 34.4402, 27.9151, 21.4286, 16.7588, 12.0463, 9.2157, 6.4479, 4.7746, 3.417, 2.6823, 1.9305, 1.6024, 1.2893, 0.8083, 0.7529, 0.5695, 0.3861, 0.492, 0.3861, 0.4247, 0.4054, 0.3921, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861], dtype=float),
        "Dislocation bubbles": np.array([15.251, 15.2124, 15.1931, 15.1737, 15.1351, 15.0965, 15.0965, 15.0579, 15.0386, 15.0253, 15.0193, 14.9566, 14.9421, 14.9095, 14.9035, 14.8649, 14.8649, 14.8649, 14.8263, 14.8456, 14.8649, 14.9839, 15.0965, 15.5318, 16.2548, 16.8103, 17.9537, 19.5282, 21.4286, 24.0486, 26.139, 29.4062, 32.471, 35.1888, 38.8996, 42.7152, 48.7066, 55.7746, 62.9344, 69.7237, 76.1004, 78.8404, 82.3674, 86.2684, 89.6367, 91.5387, 92.8185, 93.7637, 94.4015, 94.844, 95.3282, 95.5403, 95.6757, 95.8098, 95.9459, 96.0002, 96.0878, 96.1004, 96.139, 96.1776, 96.2062, 96.1999, 96.2162, 96.2355, 96.2548, 96.2219, 96.2212, 96.2355, 96.2355], dtype=float),
        "Grain boundary bubbles": np.array([2.1622, 2.1947, 2.1622, 2.1525, 2.1622, 2.1236, 2.1236, 2.1236, 2.1236, 2.1266, 2.1236, 2.1205, 2.1236, 2.1236, 2.1236, 2.1236, 2.0849, 2.1042, 2.1236, 2.0656, 2.0077, 1.9137, 1.5444, 1.412, 1.3803, 1.4149, 1.2487, 1.0877, 1.1197, 1.1019, 1.1583, 1.11, 1.1197, 1.1197, 1.1147, 1.11, 1.1197, 1.1724, 1.1969, 1.209, 1.1969, 1.1969, 1.2162, 1.1969, 1.1969, 1.1969, 1.1583, 1.1257, 1.1583, 1.1197, 1.0811, 1.0425, 1.1197, 1.0425, 1.0425, 1.0008, 1.0039, 0.9978, 0.9653, 0.9653, 0.9459, 0.9266, 0.9266, 0.9073, 0.888, 0.888, 0.888, 0.8977, 0.9073], dtype=float),
        "FGR": np.array([0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.5241, 0.8108, 0.8784, 1.0425, 1.0039, 1.3127, 1.4937, 1.4872, 1.6876, 1.8533, 1.8147, 2.0463, 2.0656, 2.1622, 2.2587, 2.3552, 2.4903, 2.5869, 2.6963, 2.8185, 2.8813, 2.8958, 2.9488, 3.0116, 3.0502, 2.973, 3.0888, 3.1274, 3.166, 3.2046, 3.2046, 3.2046, 3.2239, 3.2432, 3.2432, 3.2819, 3.2819, 3.3205, 3.3205, 3.3205, 3.3591, 3.3591, 3.3591, 3.3591, 3.3591, 3.3591, 3.3591, 3.3591], dtype=float),
    },
    3.2: {
        "T_K": np.array([900.0, 925.0, 950.0, 975.0, 1000.0, 1025.0, 1050.0, 1075.0, 1100.0, 1125.0, 1150.0, 1175.0, 1200.0, 1225.0, 1250.0, 1275.0, 1300.0, 1325.0, 1350.0, 1375.0, 1400.0, 1425.0, 1450.0, 1475.0, 1500.0, 1525.0, 1550.0, 1575.0, 1600.0, 1625.0, 1650.0, 1675.0, 1700.0, 1725.0, 1750.0, 1775.0, 1800.0, 1825.0, 1850.0, 1875.0, 1900.0, 1925.0, 1950.0, 1975.0, 2000.0, 2025.0, 2050.0, 2075.0, 2100.0, 2125.0, 2150.0, 2175.0, 2200.0, 2225.0, 2250.0, 2275.0, 2300.0, 2325.0, 2350.0, 2375.0, 2400.0, 2425.0, 2450.0, 2475.0, 2500.0, 2525.0, 2550.0, 2575.0, 2600.0], dtype=float),
        "Matrix": np.array([2.6255, 2.6255, 2.6401, 2.5982, 2.5483, 2.5097, 2.471, 2.4627, 2.4364, 2.4623, 2.4395, 2.4326, 2.4284, 2.4022, 2.3938, 2.3327, 2.3166, 2.3434, 2.4825, 2.3552, 2.3166, 2.3021, 2.2008, 2.0849, 1.9305, 1.7535, 1.4649, 1.0425, 0.8108, 0.6757, 0.5792, 0.5019, 0.4633, 0.4633, 0.4247, 0.4247, 0.4247, 0.4247, 0.4247, 0.4247, 0.4247, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861], dtype=float),
        "Bulk bubbles": np.array([88.8, 88.8687, 88.9333, 88.9812, 89.0, 89.0, 89.0, 89.0, 89.0, 89.0, 89.0, 89.0, 89.0, 89.0086, 89.0312, 89.0633, 89.1, 89.1367, 89.1688, 89.1914, 89.2, 89.1938, 89.1757, 89.1465, 89.1069, 89.0579, 89.0, 88.8962, 88.7018, 88.4065, 88.0, 86.8122, 85.0, 83.0534, 81.0, 78.9657, 74.8649, 70.1761, 64.2471, 56.562, 48.8417, 42.4687, 34.6525, 28.7452, 22.8571, 18.1425, 13.5135, 10.6564, 7.8378, 6.1669, 4.749, 3.7162, 3.0116, 2.4388, 1.9348, 1.5042, 1.1518, 0.8822, 0.7, 0.5738, 0.4712, 0.3955, 0.35, 0.3222, 0.298, 0.278, 0.2629, 0.2533, 0.25], dtype=float),
        "Dislocation bubbles": np.array([6.7568, 6.7568, 6.7181, 6.6988, 6.6795, 6.6795, 6.6409, 6.612, 6.6023, 6.5637, 6.5251, 6.5251, 6.5251, 6.4686, 6.4575, 6.4286, 6.4093, 6.4093, 6.3807, 6.3707, 6.3707, 6.39, 6.4479, 6.5782, 6.7568, 6.9719, 7.3745, 7.8736, 8.6873, 9.6435, 10.5792, 11.8383, 13.2625, 14.4156, 16.2741, 17.6938, 21.9691, 25.681, 32.1622, 39.5969, 46.2934, 53.6838, 60.1931, 65.7035, 70.8494, 75.4363, 79.9614, 82.7536, 85.2317, 87.5945, 88.8803, 89.9364, 91.0039, 91.4443, 91.9305, 92.2418, 92.471, 92.7314, 92.8571, 92.9987, 93.0502, 93.1178, 93.2046, 93.2239, 93.2819, 93.2847, 93.3205, 93.3205, 93.3205], dtype=float),
        "Grain boundary bubbles": np.array([2.5483, 2.5097, 2.5097, 2.5097, 2.5097, 2.5097, 2.471, 2.4807, 2.4324, 2.471, 2.4324, 2.4324, 2.4324, 2.4324, 2.4517, 2.4324, 2.4324, 2.4324, 2.278, 1.7448, 1.6602, 1.4607, 1.5444, 1.5444, 1.3514, 1.1583, 1.1969, 1.2268, 1.2741, 1.3401, 1.3514, 1.4358, 1.4865, 1.5058, 1.5444, 1.6409, 1.7375, 1.9813, 2.1236, 2.2524, 2.4324, 2.5686, 2.6917, 2.7992, 2.8571, 2.9585, 2.973, 3.0381, 3.0116, 3.0792, 3.0888, 3.0888, 3.0792, 3.0888, 3.0888, 3.0888, 3.0888, 3.0858, 3.0695, 3.0502, 3.0502, 3.0502, 3.0475, 3.0199, 3.0116, 3.0279, 3.0116, 3.0116, 3.0116], dtype=float),
        "FGR": np.array([0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.5019, 0.7641, 0.9653, 1.0618, 1.1197, 1.1728, 1.39, 1.448, 1.6023, 1.5927, 1.7375, 1.8349, 1.8919, 2.0206, 2.0077, 2.0994, 2.2008, 2.3301, 2.5097, 2.7102, 2.8958, 3.0502, 3.2432, 3.3784, 3.4363, 3.668, 3.7066, 3.7838, 3.861, 3.8996, 3.9382, 3.9382, 3.9768, 4.112, 4.0541, 4.0541, 4.0541, 4.0541, 4.0541, 4.0541, 4.0541, 4.0541, 4.0541, 4.0541, 4.0541, 4.0541, 4.0541, 4.0541, 4.0541, 4.0927, 4.0927], dtype=float),
    },
}

def rizk2025_partition_reference_for_burnup(bu: float):
    """Return digitized Rizk 2025 gas partition for 1.1 or 3.2% FIMA."""
    if abs(float(bu) - 1.1) < 1e-6:
        return RIZK2025_GAS_PARTITION[1.1]
    if abs(float(bu) - 3.2) < 1e-6:
        return RIZK2025_GAS_PARTITION[3.2]
    return None

PARTITION_PLOT_STYLES = {
    "Matrix": {"color": "tab:blue", "model_label": "Model Matrix (solution)", "ref_label": "Rizk 2025 Matrix"},
    "Bulk bubbles": {"color": "tab:orange", "model_label": "Model Bulk bubbles", "ref_label": "Rizk 2025 Bulk"},
    "Dislocation bubbles": {"color": "tab:green", "model_label": "Model Dislocation bubbles", "ref_label": "Rizk 2025 Dislocation"},
    "Grain boundary bubbles": {"color": "gold", "model_label": "Model Grain-face bubbles", "ref_label": "Rizk 2025 GB bubbles"},
    "FGR": {"color": "tab:red", "model_label": "Model FGR / released", "ref_label": "Rizk 2025 FGR"},
}


def plot_rizk2025_partition_dashed_lines(bu: float):
    """Overlay dashed Rizk 2025 Fig. 9 partition curves with the same colors as the model curves.

    IMPORTANT: Rizk curves are individual fractions from 0 to 100, not stacked/cumulative.
    """
    ref = rizk2025_partition_reference_for_burnup(bu)
    if ref is None:
        return

    T_ref = ref["T_K"]
    for key, style in PARTITION_PLOT_STYLES.items():
        plt.plot(
            T_ref,
            ref[key],
            "--",
            color=style["color"],
            linewidth=2.0,
            label=style["ref_label"],
        )



def _is_diagnostic_output_name(name):
    """Route auxiliary diagnostics out of the thesis-figure directory."""
    name = Path(name).name.lower()
    diagnostic_tokens = (
        "_pressure_", "_capture_", "_fcap_", "_f_cap_",
        "_diffusiv", "_rho_d_surface", "_guard_", "_guards_",
        "_vacancy_", "_psi_", "_zeta_", "_monte_resegone_",
    )
    return any(token in name for token in diagnostic_tokens)


def ensure_output_dir(path=OUTPUT_DIR):
    output_path = Path(path)
    output_path.mkdir(parents=True, exist_ok=True)
    if output_path == Path(OUTPUT_DIR):
        diagnostic_path = output_path / "diagnostic"
        diagnostic_path.mkdir(parents=True, exist_ok=True)
        # Also move existing diagnostic outputs from previous runs, if any.
        # Never delete a file when the target name is already occupied.
        for old_file in output_path.iterdir():
            if not old_file.is_file() or not _is_diagnostic_output_name(old_file.name):
                continue
            target = diagnostic_path / old_file.name
            if target.exists():
                idx = 1
                while True:
                    alternative = diagnostic_path / f"{old_file.stem}_old_{idx}{old_file.suffix}"
                    if not alternative.exists():
                        target = alternative
                        break
                    idx += 1
            old_file.rename(target)


def output_path_for(name):
    """Return the root path for results or the diagnostic subdirectory."""
    ensure_output_dir()
    base = Path(OUTPUT_DIR)
    if _is_diagnostic_output_name(name):
        base = base / "diagnostic"
    return base / name

def make_candidate(label=CASE_LABEL) -> Candidate:
    return Candidate(label=label, **dict(MANUAL_PARAMS))

# ---- Optional rho_d models ----
RHO_FAB_DEFAULT = 3.0e13
C1_RB = 1.6e14
F0_RB = 2.4
TREF_RB = 1025.0
RHO_SAT_RHO940 = 6.3571
RHO_SAT_RHOINF = 9.1036
RHO_SAT_TAU_K = 203.76

# Rizk 2023 empirical dislocation-density model.
# Eq. form: rho = rho0 * exp(c*F / max(Td, T0 - T)), capped at rho_cap.
# F must be the FIMA fraction, not the percent value.
def rho_rizk2023(T: float, burnup_percent: float) -> float:
    F_fima_fraction = float(burnup_percent) / 100.0
    denominator_K = max(float(RIZK2023_TD), float(RIZK2023_T0) - float(T))
    exponent = float(RIZK2023_C) * F_fima_fraction / denominator_K
    rho = float(RIZK2023_RHO0) * math.exp(exponent)
    return min(rho, float(RIZK2023_RHO_CAP))


# ---- Rizk 2023/Centipede digitized rho_d(F,T) fit ----
F_GRID = np.array([0.0, 0.25, 0.5, 0.75, 1.0, 1.25, 1.5, 1.75, 2.0, 2.25, 2.5, 2.75, 3.0, 3.25, 3.5, 3.75, 4.0, 4.25, 4.5, 4.75, 5.0, 5.25, 5.5, 5.75, 6.0, 6.25, 6.5, 6.75, 7.0, 7.25, 7.5, 7.75, 8.0, 8.25, 8.5, 8.75, 9.0, 9.25, 9.5, 9.75, 10.0, 10.25, 10.5, 10.75, 11.0, 11.25, 11.5, 11.75, 12.0, 12.25, 12.5, 12.75, 13.0], dtype=float)
T_SLICES = np.array([1000.0, 1200.0, 1400.0, 1600.0, 1800.0], dtype=float)

RHO_CURVES = {
    1000.0: np.array([181208053691275.2, 261169902929109.03, 326019617965926.7, 374176759288834.7, 416623644811564.2, 460786278949972.25, 503355704697986.5, 543016112222848.1, 581569437274135.2, 620662430843226.6, 659008776458440.9, 695841623278697.6, 731414558595766.6, 765403939467177.2, 798786783686112.5, 832472896231285.5, 866159008776458.4, 900171926891099.2, 933531233866804.2, 965002956949891.8, 995869901910170.4, 1027959164192910.0, 1058208569953536.4, 1083449465924263.4, 1107382550335570.4, 1132476888726398.8, 1157718120805369.0, 1182885906040268.2, 1208053691275167.8, 1233920581655481.0, 1258389261744966.2, 1279082774049216.8, 1298657718120805.2, 1318791946308724.8, 1338926174496644.2, 1359060402684563.8, 1379194630872483.2, 1399328859060402.8, 1419463087248322.0, 1439597315436241.5, 1459731543624161.0, 1480584851390220.5, 1500000000000000.0, 1515124552012538.2, 1530201342281879.2, 1550036066064961.2, 1569953536396489.2, 1585837147806939.8, 1600671140939597.2, 1615409246349507.8, 1630872483221476.8, 1648724611134386.2, 1648724611134386.2], dtype=float),
    1200.0: np.array([100671140939597.31, 160452558987948.7, 225348477026329.38, 296660507853147.9, 371321631388745.44, 446244192049561.2, 521166752710376.9, 594889344082562.8, 667139907072793.0, 736163417866146.4, 805369127516778.5, 886403777429704.4, 952891068662880.6, 981334490642886.8, 1012132163138874.4, 1114519310626566.4, 1239029426948889.8, 1320062985066368.8, 1391584925141972.0, 1460633217844681.5, 1528652555498193.0, 1601243263000018.0, 1672302529685080.0, 1736334546345922.2, 1798141455859576.8, 1861208052258096.0, 1922818791946308.8, 1980123902942694.8, 2037429013939080.5, 2101259726674828.8, 2162106350025812.8, 2209834593382154.0, 2256582343830665.0, 2313829499678579.5, 2371192565823438.0, 2419406518330112.0, 2466442953020134.0, 2518171237902952.0, 2570211667527103.5, 2619773625834206.0, 2667785234899328.5, 2713946573257701.0, 2759163655136809.0, 2804174443694320.0, 2848993288590604.0, 2893619122376745.0, 2938822922044398.0, 2959215281362932.0, 2979607640681466.0, 3000000000000000.0, 3000000000000000.0, 3000000000000000.0, 3000000000000000.0], dtype=float),
    1400.0: np.array([120805369127516.77, 263006539033238.25, 406556530717604.5, 567863722028916.4, 697728446050593.8, 773388993383370.6, 810586093524984.2, 813980257279062.4, 815171317555634.0, 815171317555634.0, 815171317555634.0, 815171317555634.0, 815171317555634.0, 815171317555634.0, 815171317555634.0, 815303779583186.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.1, 815436241610738.2, 815436241610738.2], dtype=float),
    1600.0: np.array([90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58], dtype=float),
    1800.0: np.array([50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66], dtype=float)
}

FIG411_T = np.array([1000.0, 1010.0, 1020.0, 1030.0, 1040.0, 1050.0, 1060.0, 1070.0, 1080.0, 1090.0, 1100.0, 1110.0, 1120.0, 1130.0, 1140.0, 1150.0, 1160.0, 1170.0, 1180.0, 1190.0, 1200.0, 1210.0, 1220.0, 1230.0, 1240.0, 1250.0, 1260.0, 1270.0, 1280.0, 1290.0, 1300.0, 1310.0, 1320.0, 1330.0, 1340.0, 1350.0, 1360.0, 1370.0, 1380.0, 1390.0, 1400.0, 1410.0, 1420.0, 1430.0, 1440.0, 1450.0, 1460.0, 1470.0, 1480.0, 1490.0, 1500.0, 1510.0, 1520.0, 1530.0, 1540.0, 1550.0, 1560.0, 1570.0, 1580.0, 1590.0, 1600.0, 1610.0, 1620.0, 1630.0, 1640.0, 1650.0, 1660.0, 1670.0, 1680.0, 1690.0, 1700.0, 1710.0, 1720.0, 1730.0, 1740.0, 1750.0, 1760.0, 1770.0, 1780.0, 1790.0, 1800.0], dtype=float)
FIG411_NORMAL = np.array([1238475361116869.8, 1206337687469762.5, 1173819890801022.8, 1140921971110651.5, 1103900292579539.2, 1071751100052987.8, 1042881497059395.6, 1018905772275045.4, 1009279280168768.8, 1012806251350726.8, 1038959044349880.8, 1086989609583554.0, 1164274558990539.8, 1255222815757060.5, 1358195289791762.8, 1464662325664722.0, 1582389293675001.0, 1696864500065623.2, 1815302492939039.2, 1938245467553644.8, 2059809596413371.5, 2186839463820598.0, 2332739554877922.0, 2493570412815698.0, 2654799810222290.0, 2817095956383626.0, 2960206583514625.0, 3085054308231974.0, 3183770904787842.0, 3238297996791840.5, 3207847433577905.0, 3103706625722913.0, 2930258834476760.0, 2692509540684900.0, 2437700514402336.5, 2179489306379048.0, 1920357113814714.0, 1669195322395927.5, 1424536574663567.2, 1182259008646188.8, 968550857726628.2, 794358996089738.8, 666341484684512.8, 568211961742959.6, 502287021154946.2, 452830188679245.75, 408805031446541.25, 369272237196765.8, 332434860736747.8, 300089847259658.8, 270440251572327.3, 238095238095238.38, 212039532794250.0, 187780772686433.28, 168912848158131.34, 157232704402515.84, 142857142857142.97, 132075471698113.31, 119496855345912.06, 107816711590296.6, 97933513027852.77, 88948787061994.7, 79964061096136.64, 76370170709793.42, 75471698113207.61, 74573225516621.81, 69182389937106.98, 62893081761006.34, 57502246181491.52, 54806828391734.1, 56603773584905.71, 56603773584905.71, 56603773584905.71, 56603773584905.71, 56603773584905.71, 56603773584905.71, 56603773584905.71, 56603773584905.71, 56603773584905.69, 56603773584905.695, 56603773584905.695], dtype=float)
FIG411_SHIFTED = np.array([842767295597484.2, 823899371069182.4, 795597484276729.6, 773584905660377.4, 745283018867924.6, 743396226415094.2, 701886792452830.1, 701886792452830.1, 683962264150943.2, 679245283018867.9, 694339622641509.4, 716981132075471.6, 730458221024258.8, 805031446540880.5, 830188679245283.0, 905660377358490.6, 932614555256064.8, 1009433962264151.0, 1066037735849056.4, 1122641509433962.4, 1194339622641509.2, 1264150943396226.2, 1330188679245283.0, 1452830188679245.5, 1567924528301886.8, 1665768194070080.8, 1739279588336192.0, 1816981132075472.0, 1905660377358490.8, 1935849056603773.2, 1949685534591195.2, 1903563941299790.5, 1822102425876011.2, 1747169811320754.8, 1617924528301886.8, 1473413379073756.2, 1485062893081761.0, 1286037735849056.8, 1091737150292778.0, 932075471698113.2, 781354051054384.0, 675471698113207.5, 600000000000000.0, 533892101892775.7, 473217237387065.75, 420000000000000.0, 372225988700564.94, 327254237288135.6, 286169491525423.7, 250056497175141.2, 220000000000000.0, 194666666666666.62, 172000000000000.0, 152000000000000.03, 134666666666666.66, 120000000000000.0, 107052121212121.2, 95156363636363.64, 84734545454545.45, 76208484848484.86, 70000000000000.01, 65318636363636.37, 61076363636363.64, 57248636363636.37, 53810909090909.09, 50738636363636.37, 48007272727272.734, 45592272727272.734, 43469090909090.91, 41613181818181.81, 40000000000000.0, 38505000000000.0, 37040000000000.0, 35635000000000.0, 34320000000000.004, 33125000000000.0, 32080000000000.0, 31215000000000.0, 30560000000000.0, 30145000000000.0, 30000000000000.0], dtype=float)


def _interp_F_curve(F_percent, T_slice):
    F = np.asarray(F_percent, dtype=float)
    y = RHO_CURVES[float(T_slice)]
    return np.interp(F, F_GRID, y, left=y[0], right=y[-1])


def rho_base_fig410(F_percent, T_K):
    F = np.asarray(F_percent, dtype=float)
    T = np.asarray(T_K, dtype=float)
    F_b, T_b = np.broadcast_arrays(F, T)
    flatF = F_b.ravel()
    flatT = T_b.ravel()
    vals_by_T = np.vstack([_interp_F_curve(flatF, Ts) for Ts in T_SLICES])
    out = np.empty_like(flatF, dtype=float)
    for i, Ti in enumerate(flatT):
        out[i] = np.interp(Ti, T_SLICES, vals_by_T[:, i], left=vals_by_T[0, i], right=vals_by_T[-1, i])
    return out.reshape(F_b.shape)


def _interp_fig411(T_K, variant):
    T = np.asarray(T_K, dtype=float)
    if variant == "normal":
        y = FIG411_NORMAL
    elif variant == "shifted":
        y = FIG411_SHIFTED
    else:
        raise ValueError("variant must be 'normal' or 'shifted'")
    return np.interp(T, FIG411_T, y, left=y[0], right=y[-1])


def _correction_factor(T_K, variant):
    T = np.asarray(T_K, dtype=float)
    base_ref = rho_base_fig410(np.full_like(T, 6.8, dtype=float), T)
    target_ref = _interp_fig411(T, variant)
    return np.clip(target_ref / np.maximum(base_ref, 1.0e10), 0.2, 4.0)


def _burnup_blend_weight(F_percent, F_ref=6.8):
    F = np.asarray(F_percent, dtype=float)
    return np.clip(F / F_ref, 0.0, 1.0) ** 0.7


def rho_d_rizk2023_centipede(F_percent, T_K, variant="shifted", rho_cap=4.0e15):
    """
    Return dislocation line density [m^-2].
    """
    base = rho_base_fig410(F_percent, T_K)
    if variant == "fig410":
        rho = base
    elif variant in ("normal", "shifted"):
        F = np.asarray(F_percent, dtype=float)
        T = np.asarray(T_K, dtype=float)
        F_b, T_b = np.broadcast_arrays(F, T)
        C = _correction_factor(T_b, variant)
        w = _burnup_blend_weight(F_b)
        rho = base * (1.0 + w * (C - 1.0))
    else:
        raise ValueError("variant must be 'fig410', 'normal', or 'shifted'")
    return np.clip(rho, 1.0e12, rho_cap)




def rho_rizk2023_smooth_monotone(T: float, burnup_percent: float) -> float:
    """
    Smooth Rizk-2023-inspired dislocation density [m^-2].

    Purpose: keep the qualitative idea that rho_d decreases at high temperature,
    but remove the artificial mid-temperature peak/kink from the digitized Fig. 4.10+4.11 fit.

    rho = rho_fab + scale*rho_amp*(1-exp(-F/Fc))*f_T(T)
    f_T = f_min + (1-f_min)/(1+exp((T-T_half)/width))

    This is smooth, continuous, monotonic decreasing in T, and monotonic increasing/saturating in burnup.
    """
    F = max(float(burnup_percent), 0.0)
    T = float(T)
    rho_fab = float(RHO_RIZK2023_SMOOTH_RHO_FAB)
    amp = float(RHO_RIZK2023_SMOOTH_RHO_AMP)
    scale = float(RHO_RIZK2023_SMOOTH_SCALE)
    Fc = max(float(RHO_RIZK2023_SMOOTH_FC_PERCENT), 1.0e-12)
    T_half = float(RHO_RIZK2023_SMOOTH_T_HALF)
    width = max(float(RHO_RIZK2023_SMOOTH_WIDTH), 1.0e-12)
    f_min = min(max(float(RHO_RIZK2023_SMOOTH_F_MIN), 0.0), 1.0)
    cap = float(RHO_RIZK2023_SMOOTH_CAP)

    burnup_part = 1.0 - math.exp(-F / Fc)
    # overflow-safe logistic for high-T decrease
    x = (T - T_half) / width
    if x > 80.0:
        logistic_decrease = 0.0
    elif x < -80.0:
        logistic_decrease = 1.0
    else:
        logistic_decrease = 1.0 / (1.0 + math.exp(x))
    fT = f_min + (1.0 - f_min) * logistic_decrease
    rho = rho_fab + scale * amp * burnup_part * fT
    return min(max(rho, 1.0e10), cap)

def rho_sat_shape10(T: float) -> float:
    return RHO_SAT_RHOINF - (RHO_SAT_RHOINF - RHO_SAT_RHO940) * math.exp(-(float(T) - 940.0) / RHO_SAT_TAU_K)

def rho_sat_factor_raw(T: float) -> float:
    return rho_sat_shape10(float(T)) / rho_sat_shape10(TREF_RB)

def rho_burnup_1025(F_a_o: float) -> float:
    rho_bu = C1_RB * max(float(F_a_o) - F0_RB, 0.0)
    return max(RHO_FAB, rho_bu)

def effective_rho_d(T: float, burnup: float, cand: Candidate) -> float:
    if RHO_MODE == "constant":
        return cand.rho_d
    if RHO_MODE == "rhoSat_RayBlank":
        return max(rho_burnup_1025(burnup) * RHO_SCALE * rho_sat_factor_raw(T), 1.0e10)
    if RHO_MODE == "rizk2023":
        return max(rho_rizk2023(T, burnup), 1.0e10)
    if RHO_MODE == "rizk2023_fit":
        rho_fit = rho_d_rizk2023_centipede(
            F_percent=burnup,
            T_K=T,
            variant=RHO_RIZK2023_FIT_VARIANT,
            rho_cap=RHO_RIZK2023_FIT_CAP,
        )
        return max(float(RHO_RIZK2023_FIT_SCALE) * float(rho_fit), 1.0e10)
    if RHO_MODE == "rizk2023_smooth":
        return max(rho_rizk2023_smooth_monotone(T, burnup), 1.0e10)
    raise ValueError(f"Unknown RHO_MODE={RHO_MODE!r}")

def run_model_point(T: float, burnup: float, cand: Candidate, dt_h=DT_H, n_modes=N_MODES, keep_history=False):
    rho_eff = effective_rho_d(T, burnup, cand)
    cache_key = (
        cand, float(T), float(burnup), float(dt_h), int(n_modes), bool(keep_history),
        USE_PHI_GAS_RESOLUTION, USE_NUCLEATION_MASS_COUPLING, USE_BULK_DISLOCATION_CAPTURE,
        USE_DYNAMIC_RHO_D_NUCLEATION, USE_INTERGRANULAR_MODEL, USE_LENTICULAR_COS3,
        A20_VU_DEFAULT, RHO_MODE, RHO_SCALE, RHO_RIZK2023_FIT_VARIANT,
        RHO_RIZK2023_FIT_SCALE, RHO_RIZK2023_FIT_CAP,
        RHO_RIZK2023_SMOOTH_SCALE, RHO_RIZK2023_SMOOTH_RHO_FAB, RHO_RIZK2023_SMOOTH_RHO_AMP,
        RHO_RIZK2023_SMOOTH_FC_PERCENT, RHO_RIZK2023_SMOOTH_T_HALF, RHO_RIZK2023_SMOOTH_WIDTH,
        RHO_RIZK2023_SMOOTH_F_MIN, RHO_RIZK2023_SMOOTH_CAP,
        XE_DIFFUSIVITY_MODE, VU_DIFFUSIVITY_MODE,
        DV_GB_MODE, DV_GB_MULTIPLIER, MILLER_DV_GB_D0, MILLER_DV_GB_Q_EV,
        GF_VACANCY_ABSORPTION_ONLY, INTERGRANULAR_POST_SAT_MODE,
        USE_WS_GAS_TRAPPING_BULK, USE_WS_GAS_TRAPPING_DISLOCATION, WS_GAS_TRAPPING_MAX_FACTOR,
        USE_FIRST_GAS_PRESSURE_SEED, FIRST_GAS_PRESSURE_FACTOR,
        SEED_BULK_ON_FIRST_GAS, SEED_DISLOCATION_ON_FIRST_GAS,
        USE_EQUILIBRIUM_DIMER_SEED, SEED_GAS_ATOMS_PER_BUBBLE, SEED_PRESSURE_FACTOR,
        SEED_INITIAL_DISLOCATION_BUBBLES, SEED_NEW_BULK_BUBBLES,
        USE_ZETA_OVERFLOW_GUARD, NGF_AREAL_0, DELTA_GB, FC_SAT,
        THETA_GF_DEG, R_GF0, GAMMA_GB,
        PHI_B_SCALE_CURRENT, NUCLEATION_CLUSTER_SIZE_CURRENT,
    )
    if not keep_history and cache_key in RUN_CACHE:
        return RUN_CACHE[cache_key]

    p = UNParameters(
        temperature=float(T),
        fission_rate=cand.fission_rate,
        grain_radius=GRAIN_RADIUS,
        target_burnup_percent_fima=float(burnup),
        dt=float(dt_h) * 3600.0,
        n_modes=int(n_modes),
        xe_yield=XE_YIELD,

        f_n=cand.f_n,
        K_d=cand.K_d,
        rho_d=rho_eff,

        Dv_scale=cand.Dv_scale,
        Dv_D1_scale=cand.Dv_D1_scale,
        Dv_D2_scale=cand.Dv_D2_scale,
        Dv_dislocation_scale=cand.Dv_dislocation_scale,
        vacancy_diffusivity_mode=VU_DIFFUSIVITY_MODE,
        A20_vU=A20_VU_DEFAULT,
        B21_vU=B21_VU,
        B22_vU=B22_VU,

        Dg_scale=cand.Dg_scale,
        Dg_D1_scale=cand.Dg_D1_scale,
        Dg_D3_scale=cand.Dg_D3_scale,
        Dg_dislocation_scale=cand.Dg_dislocation_scale,
        D2_xe_scale=getattr(cand, "D2_xe_scale", 1.0),
        xe_diffusivity_mode=XE_DIFFUSIVITY_MODE,

        b_scale=cand.b_scale,
        b_bulk_scale=cand.b_bulk_scale,
        b_dislocation_scale=cand.b_dislocation_scale,

        gb_scale=cand.gb_scale,
        gd_scale=cand.gd_scale,
        gd_bubble_scale=cand.gd_bubble_scale,
        gd_line_scale=cand.gd_line_scale,
        gd_line_alpha=cand.gd_line_alpha,

        coalescence_d_scale=cand.coalescence_d_scale,
        capture_scale=cand.capture_scale,
    )

    hist, rates = solve_UN(p, keep_history=keep_history)

    # Last values.
    out = {
        "label": cand.label,
        "xe_diffusivity_mode": XE_DIFFUSIVITY_MODE,
        "vacancy_diffusivity_mode": VU_DIFFUSIVITY_MODE,
        "T": float(T),
        "burnup": float(burnup),
        "rho_d_eff": rho_eff,

        "c": hist["c"][-1],
        "mb": hist["mb"][-1],
        "md": hist["md"][-1],
        "Nb": hist["Nb"][-1],
        "Nd": hist["Nd"][-1],
        "Rb_nm": hist["Rb"][-1] * 1e9,
        "Rd_nm": hist["Rd"][-1] * 1e9,

        "swelling_b_percent": 100.0 * hist["swelling_b"][-1],
        "swelling_d_percent": 100.0 * hist["swelling_d"][-1],
        "swelling_ig_percent": 100.0 * hist["swelling_ig"][-1],
        "swelling_gf_percent": 100.0 * hist.get("swelling_gf", [0.0])[-1],
        "swelling_total_gas_percent": 100.0 * hist.get("swelling_total_gas", hist["swelling_ig"])[-1],

        "Ngf_areal": hist.get("Ngf_areal", [math.nan])[-1],
        "Ngf_vol": hist.get("Ngf_vol", [math.nan])[-1],
        "Rgf_nm": hist.get("R_gf", [math.nan])[-1] * 1e9,
        "Fc_gf": hist.get("Fc_gf", [math.nan])[-1],
        "p_gf": hist.get("p_gf", [math.nan])[-1],
        "p_gf_eq": hist.get("p_gf_eq", [math.nan])[-1],
        "p_gf_over_eq": (hist.get("p_gf", [math.nan])[-1] / hist.get("p_gf_eq", [math.nan])[-1]) if hist.get("p_gf_eq", [0.0])[-1] else math.nan,

        "p_b": hist["p_b"][-1],
        "p_b_eq": hist["p_b_eq"][-1],
        "p_d": hist["p_d"][-1],
        "p_d_eq": hist["p_d_eq"][-1],
        "p_b_over_eq": hist["p_b"][-1] / hist["p_b_eq"][-1] if hist["p_b_eq"][-1] else math.nan,
        "p_d_over_eq": hist["p_d"][-1] / hist["p_d_eq"][-1] if hist["p_d_eq"][-1] else math.nan,

        "matrix_gas_percent": hist["matrix_gas_percent"][-1],
        "bulk_gas_percent": hist["bulk_gas_percent"][-1],
        "dislocation_gas_percent": hist["dislocation_gas_percent"][-1],
        "qgb_gas_percent": hist["qgb_gas_percent"][-1],
        "grainface_gas_percent": hist.get("grainface_gas_percent", [0.0])[-1],
        "release_gas_percent": hist.get("release_gas_percent", [0.0])[-1],

        "q_gb": hist["q_gb"][-1],
        "q_gf": hist.get("q_gf", [0.0])[-1],
        "q_rel": hist.get("q_rel", [0.0])[-1],
        "generated": hist["generated"][-1],
        "retained": hist["retained"][-1],

        "lambda_d": hist["lambda_d"][-1],
        "nu_b": hist["nu_b"][-1],
        "phi_b": hist["phi_b"][-1],
        "phi_d": hist["phi_d"][-1],
        "f_cap_step": hist["f_cap_step"][-1],
        "cap_raw_step": hist["cap_raw_step"][-1],
        "capture_fraction_sum": hist["capture_fraction_sum"][-1],
        "capture_raw_sum": hist["capture_raw_sum"][-1],
        "capture_bubbles_cumulative": hist["capture_bubbles_cumulative"][-1],
        "max_f_cap_step": hist["max_f_cap_step"][-1],

        "hist": hist if keep_history else None,
        "rates": rates,
    }

    # Add rate diagnostics.
    for k, v in rates.items():
        if isinstance(v, (int, float)):
            out[k] = v

    # Geometry diagnostics.
    Rb = hist["Rb"][-1]
    Rd = hist["Rd"][-1]
    Nb = hist["Nb"][-1]
    Nd = hist["Nd"][-1]
    out["psi_b"] = Rb / wigner_seitz_delta(Nb) if Nb > 0 and math.isfinite(Rb) else math.nan
    out["psi_d"] = Rd / wigner_seitz_delta(Nd) if Nd > 0 and math.isfinite(Rd) else math.nan
    out["porosity_b"] = hist["swelling_b"][-1]
    out["porosity_d"] = hist["swelling_d"][-1]
    out["valid_single_size"] = (
        math.isfinite(out["Rd_nm"])
        and math.isfinite(out["Nd"])
        and (out["Nd"] > 1.0e18)
        and (out["psi_d"] < 0.8 if math.isfinite(out["psi_d"]) else False)
        and (out["porosity_d"] < 0.5 if math.isfinite(out["porosity_d"]) else False)
    )

    if not keep_history:
        RUN_CACHE[cache_key] = out
    return out

def temperature_grid(tmax):
    n = int(round((float(tmax) - T_MIN) / T_STEP))
    return [T_MIN + i * T_STEP for i in range(n + 1)]

def simulate_grid(cand: Candidate):
    RUN_CACHE.clear()
    rows = []
    for bu in BURNUPS:
        Ts = temperature_grid(T_MAX_DIAGNOSTIC)
        print(f"Running burnup {bu:g}% FIMA: {len(Ts)} temperature points")
        for T in Ts:
            rows.append(run_model_point(T, bu, cand, DT_H, N_MODES, keep_history=False))
    return rows

def rows_for_burnup(rows, bu):
    return sorted([r for r in rows if abs(r["burnup"] - float(bu)) < 1e-9], key=lambda r: r["T"])

def row_near(rows, bu, T):
    sub = rows_for_burnup(rows, bu)
    return min(sub, key=lambda r: abs(r["T"] - T))

def savefig(name):
    path = output_path_for(name)
    plt.savefig(path, dpi=180, bbox_inches="tight")
    if not SHOW_PLOTS:
        plt.close()
    return path

def write_csv(rows, cand):
    ensure_output_dir()
    path = Path(OUTPUT_DIR) / f"{cand.label}_grid.csv"
    df = pd.DataFrame([{k: v for k, v in r.items() if k not in ("hist", "rates")} for r in rows])
    df.to_csv(path, index=False)
    return path, df

# ---- plotting helpers ----

def plot_exp_swelling_for_burnup(bu):
    series_names = sorted({p["series"] for p in EXP_SWELLING_T if abs(p["burnup"] - bu) < 1e-9})
    for series in series_names:
        pts = [p for p in EXP_SWELLING_T if abs(p["burnup"] - bu) < 1e-9 and p["series"] == series]
        marker = "x" if "119" in series else "^"
        plt.scatter([p["T"] for p in pts], [p["swelling"] for p in pts], marker=marker, s=65, label=f"Exp P2 {series}", zorder=5)

def plot_swelling_T(rows, bu, cand):
    sub = [r for r in rows_for_burnup(rows, bu) if r["T"] <= T_MAX_SWELLING]
    Ts = [r["T"] for r in sub]
    plt.figure(figsize=(9, 5.8))
    plt.plot(Ts, [r["swelling_d_percent"] for r in sub], label="Model dislocation/P2 swelling")
    plt.plot(Ts, [r["swelling_b_percent"] for r in sub], "--", label="Model bulk swelling")
    plt.plot(Ts, [r["swelling_gf_percent"] for r in sub], "-.", label="Model intergranular/grain-face swelling")
    plot_exp_swelling_for_burnup(bu)
    plt.xlabel("Temperature [K]", fontsize=MAIN_LABEL_FONTSIZE)
    plt.ylabel("Fission gas swelling [%]", fontsize=MAIN_LABEL_FONTSIZE)
    plt.xticks(fontsize=MAIN_TICK_FONTSIZE)
    plt.yticks(fontsize=MAIN_TICK_FONTSIZE)
    plt.grid(True, alpha=0.3)
    plt.legend(fontsize=11)
    return savefig(f"{cand.label}_swelling_T_{bu:.1f}FIMA.png")

def plot_swelling_burnup_1600(cand):
    burnup_grid = [0.2, 0.5, 0.8, 1.1, 1.3, 1.6, 2.0, 2.5, 3.2, 4.0, 5.0, 6.0]
    rows_bu = [run_model_point(1600.0, bu, cand, DT_H, N_MODES, keep_history=False) for bu in burnup_grid]
    plt.figure(figsize=(9, 5.8))
    plt.plot(burnup_grid, [r["swelling_d_percent"] for r in rows_bu], label="Model dislocation/P2 swelling")
    plt.plot(burnup_grid, [r["swelling_b_percent"] for r in rows_bu], "--", label="Model bulk swelling")
    plt.plot(burnup_grid, [r["swelling_gf_percent"] for r in rows_bu], "-.", label="Model intergranular/grain-face swelling")
    plt.scatter([p["burnup"] for p in EXP_SWELLING_BURNUP_1600], [p["swelling"] for p in EXP_SWELLING_BURNUP_1600], marker="^", s=80, label="Exp P2 approx")
    plt.xlabel("Burnup [% FIMA]", fontsize=MAIN_LABEL_FONTSIZE)
    plt.ylabel("Fission gas swelling [%]", fontsize=MAIN_LABEL_FONTSIZE)
    plt.xticks(fontsize=MAIN_TICK_FONTSIZE)
    plt.yticks(fontsize=MAIN_TICK_FONTSIZE)
    plt.grid(True, alpha=0.3)
    plt.legend(fontsize=11)
    return savefig(f"{cand.label}_swelling_vs_burnup_1600K.png")

def existing_bulk_value(r, key):
    """Return finite bulk value only when the bulk population exists."""
    if r.get("Nb", 0.0) > 0.0 and r.get("Rb_nm", 0.0) > 0.0 and r.get("mb", 0.0) > 0.0:
        val = r.get(key, math.nan)
        return val if math.isfinite(val) and val > 0.0 else math.nan
    return math.nan

def existing_dislocation_value(r, key):
    """Return finite dislocation value only when the dislocation population has gas/volume."""
    if r.get("Nd", 0.0) > 0.0 and r.get("Rd_nm", 0.0) > 0.0 and r.get("md", 0.0) > 0.0:
        val = r.get(key, math.nan)
        return val if math.isfinite(val) and val > 0.0 else math.nan
    return math.nan

def plot_radius_concentration_T(rows, cand, bu=1.3):
    """Plot bulk, dislocation and intergranular grain-face R/N together."""
    sub = rows_for_burnup(rows, bu)
    Ts = [r["T"] for r in sub]
    fig, axes = plt.subplots(1, 2, figsize=(13, 4.8))

    ax = axes[0]
    ax.semilogy(Ts, [existing_dislocation_value(r, "Rd_nm") for r in sub], label="Model R_d dislocation")
    ax.semilogy(Ts, [existing_bulk_value(r, "Rb_nm") for r in sub], "--", label="Model R_b bulk")
    ax.semilogy(Ts, [r.get("Rgf_nm", math.nan) for r in sub], "-.", label="Model R_gf intergranular")
    if abs(bu - 1.3) < 1e-9:
        ax.scatter([p["T"] for p in EXP_RD_T_13], [p["R_nm"] for p in EXP_RD_T_13], marker="^", color="red", label="Rizk Fig. 8 exp")
    ax.set_xlabel("Temperature [K]", fontsize=16)
    ax.set_ylabel("Bubble radius [nm]", fontsize=16)
    ax.tick_params(axis="both", labelsize=13)
    ax.grid(True, alpha=0.3, which="both")
    ax.legend(fontsize=11)

    ax = axes[1]
    ax.semilogy(Ts, [existing_dislocation_value(r, "Nd") for r in sub], label="Model N_d dislocation")
    ax.semilogy(Ts, [existing_bulk_value(r, "Nb") for r in sub], "--", label="Model N_b bulk")
    ax.semilogy(Ts, [r.get("Ngf_vol", math.nan) for r in sub], "-.", label="Model N_gf intergranular (vol. equiv.)")
    if abs(bu - 1.3) < 1e-9:
        ax.scatter([p["T"] for p in EXP_ND_T_13], [p["N"] for p in EXP_ND_T_13], marker="^", color="red", label="Rizk Fig. 7 exp")
    ax.set_xlabel("Temperature [K]", fontsize=16)
    ax.set_ylabel("Bubble concentration [m$^{-3}$]", fontsize=16)
    ax.tick_params(axis="both", labelsize=13)
    ax.grid(True, alpha=0.3, which="both")
    ax.legend(fontsize=11, loc="upper left", bbox_to_anchor=(0.01, 0.78))

    plt.tight_layout()
    return savefig(f"{cand.label}_RbRd_NbNd_T_{bu:.1f}FIMA.png")

def plot_pressure_T(rows, cand, bu):
    sub = rows_for_burnup(rows, bu)
    Ts = [r["T"] for r in sub]
    plt.figure(figsize=(9, 5.8))
    # Mask non-existing populations; otherwise p_eq uses R_min=1e-15 and shows fake ~1e15 Pa plateaus.
    plt.semilogy(Ts, [existing_bulk_value(r, "p_b") for r in sub], label="p_b bulk")
    plt.semilogy(Ts, [existing_bulk_value(r, "p_b_eq") for r in sub], "--", label="p_b,eq bulk")
    plt.semilogy(Ts, [existing_dislocation_value(r, "p_d") for r in sub], label="p_d dislocation")
    plt.semilogy(Ts, [existing_dislocation_value(r, "p_d_eq") for r in sub], "--", label="p_d,eq dislocation")
    plt.semilogy(
        Ts,
        [r.get("p_gf", math.nan) if math.isfinite(r.get("p_gf", math.nan)) and r.get("p_gf", 0.0) > 0.0 else math.nan for r in sub],
        label="p_gf grain boundary",
    )
    plt.semilogy(
        Ts,
        [r.get("p_gf_eq", math.nan) if math.isfinite(r.get("p_gf_eq", math.nan)) and r.get("p_gf_eq", 0.0) > 0.0 else math.nan for r in sub],
        "--",
        label="p_gf,eq grain boundary",
    )
    plt.xlabel("Temperature [K]", fontsize=MAIN_LABEL_FONTSIZE)
    plt.ylabel("Pressure [Pa]", fontsize=MAIN_LABEL_FONTSIZE)
    plt.xticks(fontsize=MAIN_TICK_FONTSIZE)
    plt.yticks(fontsize=MAIN_TICK_FONTSIZE)
    plt.grid(True, alpha=0.3, which="both")
    plt.legend(fontsize=11)
    return savefig(f"{cand.label}_pressure_T_{bu:.1f}FIMA.png")

def plot_pressure_ratio_T(rows, cand, bu):
    sub = rows_for_burnup(rows, bu)
    Ts = [r["T"] for r in sub]
    plt.figure(figsize=(9, 5.8))
    bulk_ratio = [
        (r["p_b_over_eq"] if math.isfinite(r.get("p_b_over_eq", math.nan)) and not math.isnan(existing_bulk_value(r, "p_b")) else math.nan)
        for r in sub
    ]
    disl_ratio = [
        (r["p_d_over_eq"] if math.isfinite(r.get("p_d_over_eq", math.nan)) and not math.isnan(existing_dislocation_value(r, "p_d")) else math.nan)
        for r in sub
    ]
    plt.plot(Ts, bulk_ratio, label="p_b/p_b,eq")
    plt.plot(Ts, disl_ratio, label="p_d/p_d,eq")
    plt.axhline(1.0, color="black", linewidth=1, linestyle="--")
    plt.xlabel("Temperature [K]", fontsize=MAIN_LABEL_FONTSIZE)
    plt.ylabel("Pressure ratio [-]", fontsize=MAIN_LABEL_FONTSIZE)
    plt.xticks(fontsize=MAIN_TICK_FONTSIZE)
    plt.yticks(fontsize=MAIN_TICK_FONTSIZE)
    plt.grid(True, alpha=0.3)
    plt.legend(fontsize=11)
    return savefig(f"{cand.label}_pressure_ratio_T_{bu:.1f}FIMA.png")

def plot_gas_partition(rows, cand, bu):
    """Plot gas-partition fractions as direct line-to-line comparison with Rizk.

    Model curves are solid lines; Rizk digitized reference curves are dashed lines
    in the same colors. This avoids the ambiguity of stacked/cumulative areas and
    lets the user immediately see whether the model lies above or below Rizk.
    """
    sub = rows_for_burnup(rows, bu)
    Ts = [r["T"] for r in sub]

    model_series = {
        "Matrix": [r["matrix_gas_percent"] for r in sub],
        "Bulk bubbles": [r["bulk_gas_percent"] for r in sub],
        "Dislocation bubbles": [r["dislocation_gas_percent"] for r in sub],
        "Grain boundary bubbles": [r.get("grainface_gas_percent", 0.0) for r in sub],
        "FGR": [r.get("release_gas_percent", 0.0) for r in sub],
    }

    plt.figure(figsize=(10.0, 6.4))

    for key, style in PARTITION_PLOT_STYLES.items():
        plt.plot(
            Ts,
            model_series[key],
            "-",
            color=style["color"],
            linewidth=2.2,
            label=style["model_label"],
        )

    plot_rizk2025_partition_dashed_lines(bu)

    plt.xlabel("Temperature [K]", fontsize=MAIN_LABEL_FONTSIZE)
    plt.ylabel("Fraction of generated gas [%]", fontsize=MAIN_LABEL_FONTSIZE)
    plt.xticks(fontsize=MAIN_TICK_FONTSIZE)
    plt.yticks(fontsize=MAIN_TICK_FONTSIZE)
    plt.ylim(0, 100)
    plt.grid(True, alpha=0.25)
    plt.legend(fontsize=10, loc="center right", ncol=2)
    return savefig(f"{cand.label}_gas_partition_{bu:.1f}FIMA.png")



def plot_diffusivities(rows, cand):
    """Compare the active Matthews/Schneider curves with the preserved Rizk baselines."""
    case_id = DIFFUSIVITY_PLOT_CASE_ID
    sub = rows_for_case(rows, case_id)
    if not sub:
        raise ValueError(f"No rows available for diffusivity plot case {case_id!r}.")

    Ts = np.array([float(r["T"]) for r in sub], dtype=float)
    case = experiment_case(case_id)
    Fdot = float(case["fission_rate"])
    kB = 8.617333262145e-5

    # Active/new curves from the actual run.
    Dxe_active = np.array([max(float(r.get("Dg", math.nan)), 1e-300) for r in sub])
    Dv_active = np.array([max(float(r.get("Dv", math.nan)), 1e-300) for r in sub])

    # Matthews user-digitized lookup references.
    Dxe_matthews = np.array([matthews2025_DXe_lookup(T) for T in Ts], dtype=float)
    Du_matthews = np.array([matthews2025_DU_lookup(T) for T in Ts], dtype=float)
    Dv_matthews_thermal = np.array(
        [3.25e-4 * math.exp(-5.95 / (kB * T)) for T in Ts], dtype=float
    )

    # Previous 16test Rizk Xe mode: rizk2025_refit_plot with D2_xe_scale=0.
    Dxe_rizk = []
    for T in Ts:
        kBT = kB * T
        D1 = 2.967341817979e-03 * math.exp(-5.013616576464e+00 / kBT)
        D2 = math.sqrt(Fdot) * 4.498475254045e-68 * _safe_exp(
            -(-1.790671812685e+01) / kBT - 9.255235831189e-01 / (kBT**2)
        )
        D3 = 1.189275430019e-38 * Fdot
        Dxe_rizk.append(
            MANUAL_PARAMS["Dg_scale"]
            * (
                MANUAL_PARAMS["Dg_D1_scale"] * D1
                + MANUAL_PARAMS["D2_xe_scale"] * D2
                + MANUAL_PARAMS["Dg_D3_scale"] * D3
            )
        )
    Dxe_rizk = np.array(Dxe_rizk, dtype=float)

    # Previous 16test Rizk vacancy mode: rizk2025_refit_full.
    Dv_rizk = []
    Dv1_rizk = []
    for T in Ts:
        kBT = kB * T
        D1 = 1.122978768506e-02 * math.exp(-5.596873538604e+00 / kBT)
        D2 = math.sqrt(Fdot) * 7.805188680989e-28 * _safe_exp(
            -9.932675113163e-01 / kBT - 2.082395503235e-02 / (kBT**2)
        )
        Dv1_rizk.append(D1)
        Dv_rizk.append(
            MANUAL_PARAMS["Dv_scale"]
            * (
                MANUAL_PARAMS["Dv_D1_scale"] * D1
                + MANUAL_PARAMS["Dv_D2_scale"] * D2
            )
        )
    Dv_rizk = np.array(Dv_rizk, dtype=float)
    Dv1_rizk = np.array(Dv1_rizk, dtype=float)

    # GB references: show BOTH closures independently of the active selection.
    Dv_gb_rizk = 1.0e6 * (
        1.35e-2 * np.exp(-5.66 / (kB * Ts))
    )
    Dv_gb_matthews = 325.0 * np.exp(-5.95 / (kB * Ts))

    plt.figure(figsize=(11.0, 6.8))

    # Xe: new vs old.
    plt.semilogy(
        Ts, np.maximum(Dxe_active, 1e-300),
        linewidth=2.4,
        label=r"ACTIVE $D_{Xe}$: Matthews/Schneider lookup",
    )
    plt.semilogy(
        Ts, np.maximum(Dxe_rizk, 1e-300),
        "--", linewidth=1.8,
        label=r"Rizk 2025 $D_{Xe}$ (previous 16test)",
    )

    # Matrix/vacancy-mediated transport: new vs old.
    plt.semilogy(
        Ts, np.maximum(Dv_active, 1e-300),
        linewidth=2.4,
        label=r"ACTIVE $D_v^{eff}$: Matthews $D_U^{tot}$ lookup",
    )
    plt.semilogy(
        Ts, np.maximum(Dv_rizk, 1e-300),
        "--", linewidth=1.8,
        label=r"Rizk 2025 $D_v$ total (previous 16test)",
    )

    # Thermal and GB context.
    plt.semilogy(
        Ts, np.maximum(Dv_matthews_thermal, 1e-300),
        ":", linewidth=1.6,
        label=r"Matthews thermal vacancy-mediated $D_U$",
    )
    plt.semilogy(
        Ts, np.maximum(Dv1_rizk, 1e-300),
        ":", linewidth=1.6,
        label=r"Rizk 2025 thermal $D_{v,1}$",
    )
    plt.semilogy(
        Ts, np.maximum(Dv_gb_matthews, 1e-300),
        "-.", linewidth=1.5,
        label=r"Matthews $D_v^{GB}$",
    )
    plt.semilogy(
        Ts, np.maximum(Dv_gb_rizk, 1e-300),
        "-.", linewidth=1.5,
        label=r"Rizk legacy $D_v^{GB}=10^6D_{v,1}$",
    )

    # Experimental Xe overlay.
    exp_df = load_schneider_fig7_exp_points()
    if exp_df is not None and not exp_df.empty:
        for exp_name, marker in EXP_XE_MARKERS.items():
            sdf = exp_df[exp_df["experiment"] == exp_name].copy()
            sdf = sdf[(sdf["T_K"] >= min(Ts)) & (sdf["T_K"] <= max(Ts))]
            if sdf.empty:
                continue
            plt.semilogy(
                sdf["T_K"].to_numpy(dtype=float),
                np.maximum(sdf["D_m2_s"].to_numpy(dtype=float), 1e-300),
                linestyle="None",
                marker=marker,
                markersize=7,
                markeredgewidth=1.6,
                zorder=10,
                label=EXP_XE_LABELS.get(exp_name, exp_name),
            )

    plt.xlabel("Temperature [K]")
    plt.ylabel("Diffusivity [m$^2$/s]")
    plt.grid(True, alpha=0.3, which="both")
    plt.legend(fontsize=7.5, ncol=2)
    return savefig(f"{cand.label}_diffusivities_updated_vs_Rizk_{case_id.replace('.', 'p')}.png")

def plot_capture_diagnostic(rows, cand, bu):
    sub = rows_for_burnup(rows, bu)
    Ts = [r["T"] for r in sub]
    plt.figure(figsize=(9, 5.8))
    plt.semilogy(Ts, [max(r["max_f_cap_step"], 1e-300) for r in sub], label="max f_cap step, clipped fraction")
    plt.semilogy(Ts, [max(r["capture_fraction_sum"], 1e-300) for r in sub], label="sum clipped f_cap steps, diagnostic")
    plt.semilogy(Ts, [max(r["capture_raw_sum"], 1e-300) for r in sub], label="sum raw capture hazard, diagnostic")
    plt.xlabel("Temperature [K]")
    plt.ylabel("Capture diagnostic")
    plt.grid(True, alpha=0.3, which="both")
    plt.legend(fontsize=8)
    return savefig(f"{cand.label}_capture_diagnostic_{bu:.1f}FIMA.png")

def plot_rho_surface(cand):
    # Simple visualization of active rho mode.
    from mpl_toolkits.mplot3d import Axes3D  # noqa: F401
    T_grid = np.arange(900, 2101, 50)
    F_grid = np.linspace(0, 100, 40)
    TT, FF = np.meshgrid(T_grid, F_grid)
    ZZ = np.vectorize(lambda T, F: math.log10(effective_rho_d(T, F/20.0 if False else F, cand)))(TT, FF)
    fig = plt.figure(figsize=(9, 6.2))
    ax = fig.add_subplot(111, projection="3d")
    surf = ax.plot_surface(TT, FF, ZZ, alpha=0.65)
    ax.set_xlabel("Temperature [K]")
    ax.set_ylabel("Burnup [MWd/kgHM proxy]")
    ax.set_zlabel(r"$\log_{10}\rho_d$ [m$^{-2}$]")
    return savefig(f"{cand.label}_rho_d_surface.png")

def make_all_plots(rows, cand):
    saved = []
    for bu in BURNUPS:
        saved.append(plot_swelling_T(rows, bu, cand))
    saved.append(plot_swelling_burnup_1600(cand))
    # R and N diagnostics now include both bulk and dislocation bubbles.
    for bu in BURNUPS:
        saved.append(plot_radius_concentration_T(rows, cand, bu))
    for bu in BURNUPS:
        saved.append(plot_pressure_T(rows, cand, bu))
        saved.append(plot_pressure_ratio_T(rows, cand, bu))
        saved.append(plot_gas_partition(rows, cand, bu))
        saved.append(plot_capture_diagnostic(rows, cand, bu))
    saved.append(plot_diffusivities(rows, cand))
    saved.append(plot_rho_surface(cand))
    return saved

def print_summary(rows, cand):
    print("\n" + "=" * 120)
    print(f"Manual case: {cand.label}")
    print("=" * 120)
    print("Physics switches:")
    print(f"  USE_PHI_GAS_RESOLUTION       = {USE_PHI_GAS_RESOLUTION}")
    print(f"  USE_NUCLEATION_MASS_COUPLING = {USE_NUCLEATION_MASS_COUPLING}")
    print(f"  USE_BULK_DISLOCATION_CAPTURE = {USE_BULK_DISLOCATION_CAPTURE}")
    print("Key parameters:")
    print(f"  K_d                          = {cand.K_d:.6g}")
    print(f"  rho_d                        = {cand.rho_d:.6g}")
    print(f"  N_d0 = K_d*rho_d             = {cand.K_d*cand.rho_d:.6e}")
    print(f"  Xe diffusivity mode          = {XE_DIFFUSIVITY_MODE}")
    print(f"  Matrix/vacancy mode          = {VU_DIFFUSIVITY_MODE}")
    print(f"  GB vacancy mode              = {DV_GB_MODE}")
    if XE_DIFFUSIVITY_MODE == "matthews2025_fig42_lookup" or VU_DIFFUSIVITY_MODE == "matthews2025_DU_fig42_lookup":
        print(f"  Matthews lookup source Fdot  = {MATTHEWS2025_LOOKUP_FISSION_RATE:.3e} m^-3 s^-1")
        print(f"  Matthews lookup chemistry y  = {MATTHEWS2025_LOOKUP_Y:.1e}")
    print(f"  USE_FIRST_GAS_PRESSURE_SEED  = {USE_FIRST_GAS_PRESSURE_SEED}")
    print(f"  FIRST_GAS_PRESSURE_FACTOR    = {FIRST_GAS_PRESSURE_FACTOR:.6g}")
    print(f"  SEED_BULK_ON_FIRST_GAS       = {SEED_BULK_ON_FIRST_GAS}")
    print(f"  SEED_DISLOCATION_ON_FIRST_GAS= {SEED_DISLOCATION_ON_FIRST_GAS}")
    print(f"  legacy dimer seed at birth   = {USE_EQUILIBRIUM_DIMER_SEED}")
    print("\nSelected diagnostics:")
    header = f"{'bu [%]':>7s} {'T [K]':>7s} {'sw_d [%]':>10s} {'R_d [nm]':>10s} {'N_d [m^-3]':>13s} {'R_b [nm]':>10s} {'N_b [m^-3]':>13s} {'p_d/peq':>9s} {'qgb [%]':>9s}"
    print(header)
    print("-" * len(header))
    for bu in BURNUPS:
        for T in [1200.0, 1600.0, 1800.0, 2000.0]:
            if T < T_MIN or T > T_MAX_DIAGNOSTIC:
                continue
            r = row_near(rows, bu, T)
            rb = r['Rb_nm'] if r.get('Nb', 0.0) > 0.0 and r.get('Rb_nm', 0.0) > 0.0 else math.nan
            nb_ = r['Nb'] if r.get('Nb', 0.0) > 0.0 else math.nan
            print(f"{bu:7.2f} {r['T']:7.0f} {r['swelling_d_percent']:10.3g} {r['Rd_nm']:10.3g} {r['Nd']:13.3e} {rb:10.3g} {nb_:13.3e} {r['p_d_over_eq']:9.3g} {r['qgb_gas_percent']:9.3g}")
    print("=" * 120)

def main():
    ensure_output_dir()
    cand = make_candidate()
    rows = simulate_grid(cand)
    csv_path, df = write_csv(rows, cand)
    print_summary(rows, cand)
    saved = make_all_plots(rows, cand)
    print(f"\nCSV written to: {csv_path}")
    print("Plots written:")
    for p in saved:
        print(f"  - {p}")
    return rows, df, saved

def inspect_point(T=1600.0, burnup=1.3, keep_history=True):
    cand = make_candidate()
    out = run_model_point(T, burnup, cand, DT_H, N_MODES, keep_history=keep_history)
    keys = ["T", "burnup", "swelling_d_percent", "swelling_b_percent", "Rd_nm", "Nd", "p_d", "p_d_eq", "p_d_over_eq", "qgb_gas_percent", "phi_b", "phi_d", "max_f_cap_step"]
    print({k: out.get(k) for k in keys})
    print("\nrates:")
    for k in ["xe_diffusivity_mode", "vacancy_diffusivity_mode", "Dg", "Dg_dislocation", "Dv", "Dv_dislocation", "Dv1", "Dv2", "Dv_lookup_base", "Dv_nonthermal_effective", "Dg_lookup_base", "A20_vU_active", "g_b", "g_d", "b_b", "b_d", "b_b_gas", "b_d_gas", "term_bubbles", "term_dislocation"]:
        print(f"  {k:20s} = {out.get(k)}")
    return out


# ============================================================
# 16testUN — CASE-SPECIFIC RONCHI/DN1 BENCHMARK EXECUTION
# ============================================================
# The solver above remains unchanged. This block changes only how benchmark
# cases are instantiated and plotted: each experimental pin gets its own Fdot.


def experiment_case(case_id: str) -> dict:
    if case_id not in EXPERIMENT_CASES:
        raise KeyError(f"Unknown experiment case {case_id!r}. Valid: {list(EXPERIMENT_CASES)}")
    return EXPERIMENT_CASES[case_id]


def candidate_for_experiment(cand: Candidate, case_id: str) -> Candidate:
    """Return the same physical/calibration candidate with experiment-specific Fdot."""
    case = experiment_case(case_id)
    return replace(
        cand,
        label=f"{cand.label}_{case_id.replace('.', 'p')}",
        fission_rate=float(case["fission_rate"]),
    )


def run_model_point_case(T: float, case_id: str, cand: Candidate, dt_h=DT_H, n_modes=N_MODES, keep_history=False):
    """Run one material-point benchmark using the experimental pin's burnup and Fdot."""
    case = experiment_case(case_id)
    cc = candidate_for_experiment(cand, case_id)
    out = run_model_point(
        T=float(T),
        burnup=float(case["burnup"]),
        cand=cc,
        dt_h=dt_h,
        n_modes=n_modes,
        keep_history=keep_history,
    )
    # Explicit provenance in CSV/output rows.
    out["experiment_case"] = case_id
    out["linear_power_kW_m"] = float(case["linear_power_kW_m"])
    out["fuel_diameter_mm"] = float(case["fuel_diameter_mm"])
    out["fission_rate_case"] = float(case["fission_rate"])
    out["fuel_description"] = case.get("fuel", "")
    return out


def simulate_grid(cand: Candidate):
    """Run the four experimental benchmark cases separately.

    Note: this is FOUR case families, not three, because AP3.2 and AP3.8 are
    distinct 1.1% FIMA pins with different linear powers / fission rates.
    """
    RUN_CACHE.clear()
    rows = []
    Ts = temperature_grid(T_MAX_DIAGNOSTIC)
    for case_id in EXPERIMENT_CASE_ORDER:
        case = experiment_case(case_id)
        print(
            f"Running {case_id}: BU={case['burnup']:g}% FIMA, "
            f"q'={case['linear_power_kW_m']:g} kW/m, "
            f"d={case['fuel_diameter_mm']:g} mm, "
            f"Fdot={case['fission_rate']:.4e} m^-3 s^-1, "
            f"{len(Ts)} temperature points"
        )
        for T in Ts:
            rows.append(run_model_point_case(T, case_id, cand, DT_H, N_MODES, keep_history=False))
    return rows


def rows_for_case(rows, case_id: str):
    return sorted([r for r in rows if r.get("experiment_case") == case_id], key=lambda r: r["T"])


def row_near_case(rows, case_id: str, T: float):
    sub = rows_for_case(rows, case_id)
    if not sub:
        raise ValueError(f"No rows found for {case_id}")
    return min(sub, key=lambda r: abs(r["T"] - float(T)))


def experimental_swelling_points_for_case(case_id: str):
    case = experiment_case(case_id)
    bu = float(case["burnup"])
    series = case["exp_series"]
    return [
        p for p in EXP_SWELLING_T
        if abs(float(p["burnup"]) - bu) < 1e-9 and p["series"] == series
    ]


def case_info_text(case_id: str) -> str:
    case = experiment_case(case_id)
    return (
        f"Experiment: {case_id}\n"
        f"Burnup: {case['burnup']:.1f}% FIMA\n"
        f"Linear heat rate: {case['linear_power_kW_m']:.0f} kW/m\n"
        f"Fission rate: {case['fission_rate']:.2e} m$^{{-3}}$ s$^{{-1}}$\n"
        f"Fuel diameter: {case['fuel_diameter_mm']:.1f} mm"
    )


def add_case_info_box(ax, case_id: str, loc=(0.02, 0.70)):
    ax.text(
        loc[0], loc[1], case_info_text(case_id),
        transform=ax.transAxes,
        ha="left", va="top",
        fontsize=MAIN_INFOBOX_FONTSIZE,
        bbox=dict(boxstyle="round,pad=0.45", facecolor="white", edgecolor="0.35", alpha=0.92),
        zorder=20,
    )


def plot_swelling_case(rows, case_id: str, cand: Candidate):
    """Case-specific swelling plot with matched experimental points and provenance box."""
    case = experiment_case(case_id)
    sub = [r for r in rows_for_case(rows, case_id) if r["T"] <= T_MAX_SWELLING]
    Ts = [r["T"] for r in sub]
    cc = candidate_for_experiment(cand, case_id)

    fig, ax = plt.subplots(figsize=(9.4, 6.0))
    ax.plot(Ts, [r["swelling_d_percent"] for r in sub], label="Model dislocation/P2 swelling")
    ax.plot(Ts, [r["swelling_b_percent"] for r in sub], "--", label="Model bulk swelling")
    ax.plot(Ts, [r["swelling_gf_percent"] for r in sub], "-.", label="Model grain-face swelling")

    pts = experimental_swelling_points_for_case(case_id)
    marker = "x" if case_id == "AP3.8" else "^"
    if pts:
        ax.scatter(
            [p["T"] for p in pts], [p["swelling"] for p in pts],
            marker=marker, s=68,
            label=f"Exp P2 {case_id} ({case['linear_power_kW_m']:.0f} kW/m)",
            zorder=5,
        )

    ax.set_xlabel("Temperature [K]", fontsize=MAIN_LABEL_FONTSIZE)
    ax.set_ylabel("Fission gas swelling [%]", fontsize=MAIN_LABEL_FONTSIZE)
    ax.tick_params(axis="both", which="both", labelsize=MAIN_TICK_FONTSIZE)
    ax.grid(True, alpha=0.3)
    ax.legend(fontsize=MAIN_LEGEND_FONTSIZE, loc="upper left")
    add_case_info_box(ax, case_id)
    fig.tight_layout()
    return savefig(f"{cc.label}_swelling_T_{case['burnup']:.1f}FIMA.png")


def plot_swelling_case_comparison_1600(rows, cand: Candidate):
    """Replace the old arbitrary continuous-burnup sweep with actual experimental cases at 1600 K."""
    fig, ax = plt.subplots(figsize=(9.4, 5.9))
    # Small horizontal offsets only to separate AP3.2/AP3.8 at the same burnup.
    offsets = {"AP3.2": -0.035, "AP3.8": 0.035, "AP3.4": 0.0, "ANP6": 0.0}
    for case_id in EXPERIMENT_CASE_ORDER:
        case = experiment_case(case_id)
        r = row_near_case(rows, case_id, 1600.0)
        x = float(case["burnup"]) + offsets.get(case_id, 0.0)
        ax.scatter([x], [r["swelling_d_percent"]], s=75, label=f"Model {case_id}")
        ax.annotate(case_id, (x, r["swelling_d_percent"]), xytext=(4, 5), textcoords="offset points", fontsize=8)

    for p in EXP_SWELLING_BURNUP_1600:
        # Match the two 1.1% experimental points to their respective pins.
        if "100" in p["series"]:
            x = 1.1 + offsets["AP3.2"]
            lab = "Exp AP3.2"
        elif "119" in p["series"]:
            x = 1.1 + offsets["AP3.8"]
            lab = "Exp AP3.8"
        elif abs(float(p["burnup"]) - 1.31) < 0.05:
            x = 1.3
            lab = "Exp AP3.4"
        else:
            x = 3.2
            lab = "Exp ANP6"
        ax.scatter([x], [p["swelling"]], marker="x", s=75, label=lab)

    ax.set_xlabel("Burnup [% FIMA]", fontsize=MAIN_LABEL_FONTSIZE)
    ax.set_ylabel("P2/dislocation swelling [%]", fontsize=MAIN_LABEL_FONTSIZE)
    ax.tick_params(axis="both", labelsize=MAIN_TICK_FONTSIZE)
    ax.grid(True, alpha=0.3)
    ax.legend(fontsize=11, ncol=2)
    fig.tight_layout()
    return savefig(f"{cand.label}_swelling_case_specific_1600K.png")


def make_all_plots(rows, cand):
    """Case-aware plot suite. All physics outputs are evaluated with each pin's Fdot."""
    saved = []

    # Main validation plots: one swelling figure per experimental pin.
    for case_id in EXPERIMENT_CASE_ORDER:
        saved.append(plot_swelling_case(rows, case_id, cand))
    saved.append(plot_swelling_case_comparison_1600(rows, cand))

    # R/N experimental diagnostics exist for the 1.3% FIMA/AP3.4 benchmark.
    ap34_rows = rows_for_case(rows, "AP3.4")
    ap34_cand = candidate_for_experiment(cand, "AP3.4")
    saved.append(plot_radius_concentration_T(ap34_rows, ap34_cand, 1.3))

    # Remaining diagnostics are also run case-by-case so no 1.1% cases are mixed.
    for case_id in EXPERIMENT_CASE_ORDER:
        case = experiment_case(case_id)
        crows = rows_for_case(rows, case_id)
        cc = candidate_for_experiment(cand, case_id)
        bu = float(case["burnup"])
        saved.append(plot_pressure_T(crows, cc, bu))
        saved.append(plot_pressure_ratio_T(crows, cc, bu))
        saved.append(plot_gas_partition(crows, cc, bu))
        saved.append(plot_capture_diagnostic(crows, cc, bu))

    # Diffusivities are Fdot-dependent; show them for every benchmark case.
    for case_id in EXPERIMENT_CASE_ORDER:
        crows = rows_for_case(rows, case_id)
        cc = candidate_for_experiment(cand, case_id)
        # Use a local case-aware version of the diffusivity plot.
        sub = crows
        Ts = [r["T"] for r in sub]
        plt.figure(figsize=(10.2, 6.2))
        plt.semilogy(Ts, [max(r.get("Dg", math.nan), 1e-300) for r in sub], linewidth=2.0, label=r"Model $D_g^{Xe}$")
        plt.semilogy(Ts, [max(r.get("Dv", math.nan), 1e-300) for r in sub], label="Dv vacancies, bulk total")
        plt.semilogy(Ts, [max(r.get("Dv1", math.nan), 1e-300) for r in sub], "--", label="Dv1 vacancies, bulk thermal")
        plt.semilogy(Ts, [max(r.get("Dv2", math.nan), 1e-300) for r in sub], ":", label="Dv2 vacancies, bulk irradiation enhanced")
        plt.semilogy(Ts, [max(r.get("Dv_gb", math.nan), 1e-300) for r in sub], "-.", label="Dv vacancies, grain boundaries")
        plt.xlabel("Temperature [K]", fontsize=MAIN_LABEL_FONTSIZE)
        plt.ylabel("Diffusivity [m$^2$/s]", fontsize=MAIN_LABEL_FONTSIZE)
        plt.xticks(fontsize=MAIN_TICK_FONTSIZE)
        plt.yticks(fontsize=MAIN_TICK_FONTSIZE)
        plt.grid(True, alpha=0.3, which="both")
        plt.legend(fontsize=11)
        saved.append(savefig(f"{cc.label}_diffusivities_T.png"))

    saved.append(plot_rho_surface(cand))
    return saved


def print_summary(rows, cand):
    print("\n" + "=" * 145)
    print(f"Manual physics candidate: {cand.label}")
    print("Case-specific experimental inputs:")
    for case_id in EXPERIMENT_CASE_ORDER:
        c = experiment_case(case_id)
        print(
            f"  {case_id:5s}: BU={c['burnup']:.1f}% FIMA, q'={c['linear_power_kW_m']:.0f} kW/m, "
            f"d={c['fuel_diameter_mm']:.1f} mm, Fdot={c['fission_rate']:.4e} m^-3 s^-1"
        )
    print("=" * 145)
    print("Physics switches:")
    print(f"  USE_PHI_GAS_RESOLUTION       = {USE_PHI_GAS_RESOLUTION}")
    print(f"  USE_NUCLEATION_MASS_COUPLING = {USE_NUCLEATION_MASS_COUPLING}")
    print(f"  USE_BULK_DISLOCATION_CAPTURE = {USE_BULK_DISLOCATION_CAPTURE}")
    print("Key calibration parameters:")
    print(f"  f_n                          = {cand.f_n:.6g}")
    print(f"  K_d                          = {cand.K_d:.6g}")
    print(f"  rho_d                        = {cand.rho_d:.6g}")
    print(f"  Dv_dislocation_scale         = {cand.Dv_dislocation_scale:.6g}")
    print(f"  Dg_dislocation_scale         = {cand.Dg_dislocation_scale:.6g}")
    print("\nSelected diagnostics:")
    header = f"{'case':>6s} {'bu [%]':>7s} {'T [K]':>7s} {'Fdot':>11s} {'sw_d [%]':>10s} {'R_d [nm]':>10s} {'N_d [m^-3]':>13s} {'p_d/peq':>9s} {'qgb [%]':>9s} {'FGR [%]':>9s}"
    print(header)
    print("-" * len(header))
    for case_id in EXPERIMENT_CASE_ORDER:
        c = experiment_case(case_id)
        for T in [1200.0, 1600.0, 1800.0]:
            r = row_near_case(rows, case_id, T)
            print(
                f"{case_id:>6s} {c['burnup']:7.2f} {r['T']:7.0f} {c['fission_rate']:11.3e} "
                f"{r['swelling_d_percent']:10.3g} {r['Rd_nm']:10.3g} {r['Nd']:13.3e} "
                f"{r['p_d_over_eq']:9.3g} {r['qgb_gas_percent']:9.3g} {r['release_gas_percent']:9.3g}"
            )
    print("=" * 145)


def main():
    ensure_output_dir()
    cand = make_candidate()
    rows = simulate_grid(cand)
    csv_path, df = write_csv(rows, cand)
    print_summary(rows, cand)
    saved = make_all_plots(rows, cand)
    print(f"\nCSV written to: {csv_path}")
    print("Plots written:")
    for p in saved:
        print(f"  - {p}")
    return rows, df, saved


def inspect_case_point(case_id="AP3.4", T=1600.0, keep_history=True):
    cand = make_candidate()
    out = run_model_point_case(T, case_id, cand, DT_H, N_MODES, keep_history=keep_history)
    keys = [
        "experiment_case", "T", "burnup", "fission_rate_case", "swelling_d_percent",
        "swelling_b_percent", "Rd_nm", "Nd", "p_d", "p_d_eq", "p_d_over_eq",
        "qgb_gas_percent", "grainface_gas_percent", "release_gas_percent",
    ]
    print({k: out.get(k) for k in keys})
    return out
