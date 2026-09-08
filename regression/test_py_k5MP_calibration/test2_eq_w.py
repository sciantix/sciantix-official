from pathlib import Path
import shutil
import subprocess
import math
import numpy as np
import pandas as pd
from scipy.stats import qmc



# CONFIGURATION

BASE = Path(__file__).resolve().parent
ROOT = BASE.parents[1]  # sciantix-official, if this file is in regression/test_5MP_calibration

EXP_INTRA = BASE / "exp_data" / "intra"
EXP_INTER = BASE / "exp_data" / "inter"
TEMPLATES = BASE / "templates"
WORK = BASE / "work"

# tuo eseguibile, percorso/nome
SCI_EXE_MANUAL =  BASE.parent / "test_Cm" / "sciantix.x"

SCI_EXE_CANDIDATES = [
    ROOT / "sciantix.exe",
    ROOT / "sciantix.x",
    ROOT / "bin" / "sciantix.exe",
    ROOT / "bin" / "sciantix.x",
    ROOT / "build" / "sciantix.exe",
    ROOT / "build" / "sciantix.x",
]

# file output SCIANTIX
OUTPUT_NAME = "output.txt"

# colonna burnup nell'output SCIANTIX.
BURNUP_COLUMN = "Burnup (MWd/kgUO2)"


# burnup target
BURNUP_EXP = 54.10116

# Average_inclusion_radius nei file sperimentali è in micron
EXP_RADIUS_MULTIPLIER = 1.0e-6

# Se Inclusions nei file sperimentali è scritto come 2.3 per indicare 2.3%, metti True.
# Se è scritto come 0.023, lascia False.
INCLUSIONS_IS_PERCENT = False

# Volume atomico medio della fase metallica 5MP [m3/atom]
OMEGA_5MP = 1.44123e-29

# MODEL VERSION USED FOR THIS FINAL CALIBRATION
# IMPORTANT: the SCIANTIX executable pointed to by SCI_EXE_MANUAL must have
# already been compiled with:
#     y_5MP = 0.578 atoms/fission
# and with the corrected reference re-solution rate:
#     k_res_ref = 3.085078e-6 s^-1
#
# The production yield is defined in SCIANTIX C++, not in this Python script.

# GLOBAL CALIBRATION SETTINGS
# One simultaneous global calibration of all free parameters.
# 4096 Sobol points + 1 explicit nominal baseline = 4097 parameter sets total.
#
# The kinetic prefactors k are sampled uniformly in log10-space because
# they span several orders of magnitude.
# The dG values are actual activation energies/barriers in eV because the
# historical 'SF' variables multiply a baseline equal to 1. They are sampled
# linearly in energy space.

N_SOBOL_SAMPLES = 512
SOBOL_SEED = 42

# RUN MODE
# True  -> run the 12-point diagnostic sensitivity sweep
# False -> use the normal baseline/Sobol calibration logic
RUN_DIAGNOSTIC_SWEEP = False

# Used only when RUN_DIAGNOSTIC_SWEEP = True:
# True  -> nominal all-ones baseline only
# False -> baseline + 4096 Sobol points
RUN_BASELINE_ONLY = False


# FINAL 2-PARAMETER CALIBRATION
#
# The diagnostic analysis identified two effective parameters that remain
# informative and sufficiently identifiable:
#
#   1) common precipitation prefactor:
#
#          sf_k_intra = sf_k_GB = sf_k_prec
#
#   2) common CNT nucleation-barrier scale:
#
#          sf_dG_nucleation = sf_dG_nucleation_GB = sf_B_CNT
#
# All remaining scaling factors are fixed to 1.
#
# The precipitation prefactor is sampled in log10-space.
# The CNT barrier scale is sampled linearly.
#
# 512 Sobol points + 1 explicit reference point = 513 parameter sets.

# Final calibration domain
SF_K_PREC_BOUNDS = (2.5e-15, 8.0e-15)   # logarithmic
SF_B_CNT_BOUNDS = (245.0, 285.0)        # linear

# Reference point retained explicitly in the sample set.
# This is the best point found in the final diagnostic sweep.
SF_K_PREC_REFERENCE = 5.0e-15
SF_B_CNT_REFERENCE = 260.0


# FINAL CALIBRATION RANGES
#
# Only two effective parameters are free in the final calibration.
# The historical individual bounds below are no longer used.

# Fixed parameters
#
# Nucleation prefactors, precipitation activation energies and irradiation
# re-solution are not calibrated in the final step.
SF_K_NUCL_FIXED = 1.0
SF_K_NUCL_GB_FIXED = 1.0

SF_DG_INTRA_FIXED = 1.0
SF_DG_GB_FIXED = 1.0

SF_K_RES_FIXED = 1.0

# Pesi nella funzione errore
WEIGHT_INTRA_FRACTION = 1.0
WEIGHT_INTRA_RADIUS = 1.0
WEIGHT_INTER_FRACTION = 1.0
WEIGHT_INTER_RADIUS = 1.0

# Colonne sperimentali
EXP_COL_INCLUSIONS = "Inclusions"
EXP_COL_RADIUS = "Average_inclusion_radius"
EXP_COL_STD_RADIUS = "Standard_Deviation_of_inclusion_radius"
EXP_COL_SP = "SP"

# Colonne output SCIANTIX
COL_CM_PREC_INTRA = "Cm precipitated intragranular (at/m3)"
COL_CM_PREC_GB = "Cm precipitated grain boundary (at/m3)"
COL_INTRA_5MP_CONC = "Intragranular 5MPs concentration (5MP/m3)"
COL_ATOMS_PER_5MP = "Intragranular atom per 5MP (at/5MP)"
COL_INTER_5MP_CONC = "Intergranular 5MPs concentration (5MP/m3)"
COL_INTER_ATOMS_PER_5MP = "Intergranular atom per 5MP (at/5MP)"


# EXPERIMENTAL CONDITIONS
conditions = [
    {
        "name": "r000",
        "r_over_R": 0.00,
        "burnup_exp": BURNUP_EXP,
        "exp_intra": EXP_INTRA / "exp_intra_r000.txt",
        "exp_inter": EXP_INTER / "exp_inter_r000.txt",
        "tem_thickness": 50e-9,   # valore nominale, da verificare
    },
    {
        "name": "r030",
        "r_over_R": 0.30,
        "burnup_exp": BURNUP_EXP,
        "exp_intra": EXP_INTRA / "exp_intra_r030.txt",
        "exp_inter": EXP_INTER / "exp_inter_r030.txt",
        "tem_thickness": 50e-9,   # valore nominale, da verificare
    },
    {
        "name": "r056",
        "r_over_R": 0.56,
        "burnup_exp": BURNUP_EXP,
        "exp_intra": EXP_INTRA / "exp_intra_r056.txt",
        "exp_inter": EXP_INTER / "exp_inter_r056.txt",
        "tem_thickness": 70e-9, #valore esplicito in tesi
    },
    {
        "name": "r075",
        "r_over_R": 0.75,
        "burnup_exp": BURNUP_EXP,
        "exp_intra": EXP_INTRA / "exp_intra_r075.txt",
        "exp_inter": EXP_INTER / "exp_inter_r075.txt",
        "tem_thickness": 40e-9, #valore esplicito in tesi
    },
]


# UTILITY FUNCTIONS
def find_sciantix_executable():
    if SCI_EXE_MANUAL is not None:
        exe = Path(SCI_EXE_MANUAL)
        if not exe.exists():
            raise FileNotFoundError(f"SCI_EXE_MANUAL does not exist: {exe}")
        return exe

    for exe in SCI_EXE_CANDIDATES:
        if exe.exists():
            return exe

    raise FileNotFoundError(
        "Could not find SCIANTIX executable. "
        "Set SCI_EXE_MANUAL at the top of this script."
    )


def require_file(path):
    if not path.exists():
        raise FileNotFoundError(f"Missing file: {path}")


def require_dir(path):
    if not path.exists():
        raise FileNotFoundError(f"Missing folder: {path}")


def clean_column_names(df):
    df.columns = [str(c).strip() for c in df.columns]
    return df


def get_required_column(df, colname):
    if colname not in df.columns:
        print("\nAvailable columns:")
        for c in df.columns:
            print(f"  - {c}")
        raise KeyError(f"Required column not found: {colname}")
    return df[colname]


def read_table_with_tabs(path):
    """
    For SCIANTIX output.
    Assumes tab-separated columns, because column names contain spaces.
    """
    df = pd.read_csv(path, sep="\t", engine="python")
    df = clean_column_names(df)

    # If everything was read as one column, try comma-separated.
    if len(df.columns) == 1:
        df = pd.read_csv(path, sep=",", engine="python")
        df = clean_column_names(df)

    if len(df.columns) == 1:
        raise ValueError(
            f"Could not parse {path}. "
            "The script expects tab-separated columns in SCIANTIX output."
        )

    return df


# READ EXPERIMENTAL DATA
def read_exp_file(path):
    """
    Expected file format:

    Inclusions    Average_inclusion_radius    Standard_Deviation_of_inclusion_radius    SP
    0.023         45.0                        12.0                                      0.82
    """

    require_file(path)

    df = pd.read_csv(path, sep=r"\s+", engine="python")
    df = clean_column_names(df)

    if len(df) == 0:
        raise ValueError(f"Experimental file is empty: {path}")

    row = df.iloc[0]

    inclusions = float(row[EXP_COL_INCLUSIONS])
    if INCLUSIONS_IS_PERCENT:
        inclusions = inclusions / 100.0

    radius = float(row[EXP_COL_RADIUS]) * EXP_RADIUS_MULTIPLIER
    std_radius = float(row[EXP_COL_STD_RADIUS]) * EXP_RADIUS_MULTIPLIER
    sp = float(row[EXP_COL_SP])

    return {
        "inclusions": inclusions,   # area fraction, dimensionless
        "radius": radius,           # m
        "std_radius": std_radius,   # m
        "sp": sp,
    }


# PREPARE AND RUN SCIANTIX
def prepare_run_folder(condition, params):
    case_name = condition["name"]

    template_dir = TEMPLATES / case_name
    run_dir = WORK / case_name

    require_dir(template_dir)

    if run_dir.exists():
        shutil.rmtree(run_dir)

    shutil.copytree(template_dir, run_dir)

    replacements = {
    "{{SF_K_INTRA_0}}": f"{params['sf_k_intra']:.12e}",
    "{{SF_K_GB_0}}": f"{params['sf_k_GB']:.12e}",
    "{{SF_K_NUCL}}": f"{params['sf_k_nucl']:.12e}",
    "{{SF_K_RES}}": f"{params['sf_k_res']:.12e}",
    "{{SF_DG_NUCLEATION}}": f"{params['sf_dG_nucleation']:.12e}",
    "{{SF_DG_INTRA}}": f"{params['sf_dG_intra']:.12e}",
    "{{SF_DG_GB}}": f"{params['sf_dG_GB']:.12e}",
    "{{SF_DG_NUCLEATION_GB}}": f"{params['sf_dG_nucleation_gb']:.12e}",
    "{{SF_K_NUCL_GB}}": f"{params['sf_k_nucl_gb']:.12e}",
    }
    

    for file in run_dir.rglob("*"):
        if not file.is_file():
            continue

        try:
            text = file.read_text()
        except UnicodeDecodeError:
            continue

        original_text = text

        for key, value in replacements.items():
            text = text.replace(key, value)

        if text != original_text:
            file.write_text(text)

    return run_dir


def run_sciantix(run_dir, sci_exe):
    try:
        completed = subprocess.run(
            [str(sci_exe)],
            cwd=run_dir,
            check=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
        )
        
        return completed

    except subprocess.CalledProcessError as e:
        print("\nSCIANTIX failed.")
        print(f"Run directory: {run_dir}")
        print("\nSTDOUT:")
        print(e.stdout)
        print("\nSTDERR:")
        print(e.stderr)
        raise


# READ SCIANTIX OUTPUT AND BUILD MODEL TARGETS
def select_output_row(df, burnup_exp):
    if BURNUP_COLUMN is None:
        return df.index[-1]

    burnup = get_required_column(df, BURNUP_COLUMN)
    idx = (burnup - burnup_exp).abs().idxmin()
    return idx


def read_sciantix_output(run_dir, burnup_exp, tem_thickness):
    output_file = run_dir / OUTPUT_NAME
    require_file(output_file)

    df = read_table_with_tabs(output_file)
    idx = select_output_row(df, burnup_exp)

    Cm_prec_intra = float(df.loc[idx, COL_CM_PREC_INTRA])
    Cm_prec_GB = float(df.loc[idx, COL_CM_PREC_GB])

    N_intra = float(df.loc[idx, COL_INTRA_5MP_CONC])
    atoms_per_5MP = float(df.loc[idx, COL_ATOMS_PER_5MP])
    N_inter = float(df.loc[idx, COL_INTER_5MP_CONC])
    atoms_per_5MP_inter = float(df.loc[idx, COL_INTER_ATOMS_PER_5MP])

    # Intragranular mean radius from atoms per 5MP.
    if atoms_per_5MP > 0.0:
        R_intra_model = (
            (3.0 * atoms_per_5MP * OMEGA_5MP)
            / (4.0 * math.pi)
        ) ** (1.0 / 3.0)

    # Fallback: same radius reconstructed from precipitated concentration and number density.
    elif N_intra > 0.0 and Cm_prec_intra > 0.0:
        R_intra_model = (
            (3.0 * Cm_prec_intra * OMEGA_5MP)
            / (4.0 * math.pi * N_intra)
        ) ** (1.0 / 3.0)

    else:
        R_intra_model = 0.0

    # Intergranular mean radius from atoms per 5MP.
    if atoms_per_5MP_inter > 0.0:
        R_inter_model = (
            (3.0 * atoms_per_5MP_inter * OMEGA_5MP)
            / (4.0 * math.pi)
        ) ** (1.0 / 3.0)

    # Fallback: same radius reconstructed from precipitated concentration and number density.
    elif N_inter > 0.0 and Cm_prec_GB > 0.0:
        R_inter_model = (
            (3.0 * Cm_prec_GB * OMEGA_5MP)
            / (4.0 * math.pi * N_inter)
        ) ** (1.0 / 3.0)

    else:
        R_inter_model = 0.0

   
    # PROJECTED AREA FRACTIONS FOR COMPARISON WITH TEM MICROGRAPHS

    # Expected projected coverage of spherical particles through
    # a TEM foil of thickness tem_thickness.
    #
    # For small coverage:
    # A_A ≈ N * pi * R^2 * t
    #
    # The exponential form accounts approximately for overlap of
    # projected particles and guarantees 0 <= A_A <= 1.

    coverage_intra = max(N_intra, 0.0) * math.pi * max(R_intra_model, 0.0)**2 * tem_thickness
    coverage_GB = max(N_inter, 0.0) * math.pi * max(R_inter_model, 0.0)**2 * tem_thickness

    f_intra_model = -math.expm1(-coverage_intra)
    f_GB_model = -math.expm1(-coverage_GB)

    # Volume fractions retained only as diagnostic quantities
    f_intra_volume = max(Cm_prec_intra, 0.0) * OMEGA_5MP
    f_GB_volume = max(Cm_prec_GB, 0.0) * OMEGA_5MP

    selected_burnup = None
    if BURNUP_COLUMN is not None:
        selected_burnup = float(df.loc[idx, BURNUP_COLUMN])

    return {
        "selected_burnup": selected_burnup,

        # Frazioni AREALI usate nella calibrazione
        "f_intra": f_intra_model,
        "f_GB": f_GB_model,

        # Frazioni VOLUMICHE solo diagnostiche
        "f_intra_volume": f_intra_volume,
        "f_GB_volume": f_GB_volume,

        "R_intra": R_intra_model,
        "R_inter": R_inter_model,

        "Cm_prec_intra": Cm_prec_intra,
        "Cm_prec_GB": Cm_prec_GB,
        "N_intra": N_intra,
        "atoms_per_5MP": atoms_per_5MP,
        "N_inter": N_inter,
        "atoms_per_5MP_inter": atoms_per_5MP_inter,
    }


# ERROR FUNCTION
def log_ratio_error(model_value, exp_value):
    """
    Absolute logarithmic ratio error:

        e = |ln(model / experiment)|

    This metric is dimensionless and symmetric for multiplicative
    overprediction and underprediction.

    If the model predicts zero or a negative value for a positive
    experimental observable, the discrepancy is treated as infinite.
    """
    if model_value is None or not np.isfinite(model_value):
        raise ValueError(f"Non-finite model value: {model_value}")

    if exp_value is None or not np.isfinite(exp_value):
        raise ValueError(f"Invalid experimental value: {exp_value}")

    if exp_value <= 0.0:
        raise ValueError(
            f"Experimental value must be > 0 for logarithmic ratio error: {exp_value}"
        )

    if model_value <= 0.0:
        return float("inf")

    return abs(math.log(model_value / exp_value))

def compute_condition_error(model, exp_intra, exp_inter):

    # Intragranular area fraction
    err_intra_fraction = log_ratio_error(
        model["f_intra"],
        exp_intra["inclusions"]
    )

    # Intragranular radius
    err_intra_radius = log_ratio_error(
        model["R_intra"],
        exp_intra["radius"]
    )

    # GB area fraction
    # CALCOLATA SOLO PER DIAGNOSTICA, NON entra nell'errore totale
    err_inter_fraction = log_ratio_error(
        model["f_GB"],
        exp_inter["inclusions"]
    )

    # GB radius
    err_inter_radius = log_ratio_error(
        model["R_inter"],
        exp_inter["radius"]
    )

    
    # OBJECTIVE FUNCTION
    # Only 3 quantities are used at each radial position:
    # 1) intra area fraction
    # 2) intra radius
    # 3) GB radius
    #
    # The GB area fraction is retained only as a diagnostic quantity.
    
    condition_error = (
        err_intra_fraction
        + err_intra_radius
        + err_inter_radius
    )

    error_terms = {
        "err_intra_fraction": err_intra_fraction,
        "err_intra_radius": err_intra_radius,

        # kept in output only as diagnostic
        "err_inter_fraction": err_inter_fraction,

        "err_inter_radius": err_inter_radius,
    }

    return condition_error, error_terms

# PARAMETER EVALUATION
def evaluate_parameter_set(params, sci_exe):
    total_error = 0.0
    comparison_rows = []

    for condition in conditions:
        exp_intra = read_exp_file(condition["exp_intra"])
        exp_inter = read_exp_file(condition["exp_inter"])

        run_dir = prepare_run_folder(condition, params)
        run_sciantix(run_dir, sci_exe)

        model = read_sciantix_output(run_dir, condition["burnup_exp"], condition["tem_thickness"])

        # Reject a parameter set immediately if SCIANTIX produced NaN/Inf.
        # We check both the quantities entering the objective function and
        # the raw quantities used to reconstruct them.
        values_to_check = {
            "f_intra": model["f_intra"],
            "R_intra": model["R_intra"],
            "f_GB": model["f_GB"],
            "R_inter": model["R_inter"],
            "Cm_prec_intra": model["Cm_prec_intra"],
            "Cm_prec_GB": model["Cm_prec_GB"],
            "N_intra": model["N_intra"],
            "atoms_per_5MP": model["atoms_per_5MP"],
            "N_inter": model["N_inter"],
            "atoms_per_5MP_inter": model["atoms_per_5MP_inter"],
        }

        bad_values = {
            name: value
            for name, value in values_to_check.items()
            if value is None or not np.isfinite(value)
        }

        if bad_values:
            raise ValueError(
                f"Non-finite SCIANTIX result at {condition['name']}: "
                f"{bad_values}"
            )

        condition_error, error_terms = compute_condition_error(
            model,
            exp_intra,
            exp_inter
        )

        total_error += condition_error

        comparison_rows.append({
            "case": condition["name"],
            "r_over_R": condition["r_over_R"],
            "burnup_exp": condition["burnup_exp"],
            "burnup_model_selected": model["selected_burnup"],

            "tem_thickness_m": condition["tem_thickness"],

            "exp_intra_inclusions": exp_intra["inclusions"],
            "model_intra_area_fraction": model["f_intra"],
            "model_intra_volume_fraction": model["f_intra_volume"],
            "exp_intra_radius_m": exp_intra["radius"],
            "model_intra_radius_m": model["R_intra"],

            "exp_inter_inclusions": exp_inter["inclusions"],
            "model_GB_area_fraction": model["f_GB"],
            "model_GB_volume_fraction": model["f_GB_volume"],
            "exp_inter_radius_m": exp_inter["radius"],
            "model_inter_radius_m": model["R_inter"],

            "err_intra_fraction": error_terms["err_intra_fraction"],
            "err_intra_radius": error_terms["err_intra_radius"],
            "err_inter_fraction": error_terms["err_inter_fraction"],
            "err_inter_radius": error_terms["err_inter_radius"],
            "condition_error": condition_error,

            "Cm_prec_intra": model["Cm_prec_intra"],
            "Cm_prec_GB": model["Cm_prec_GB"],
            "N_intra": model["N_intra"],
            "atoms_per_5MP": model["atoms_per_5MP"],
            "N_inter": model["N_inter"],
            "atoms_per_5MP_inter": model["atoms_per_5MP_inter"],
        })

    number_of_error_terms = len(conditions) * 3  # 4 positions × 3 observables = 12
    total_error /= number_of_error_terms

    return total_error, comparison_rows



# GLOBAL PARAMETER SAMPLING
def log_sample(u, bounds):
    """
    Map u in [0, 1] to a value sampled uniformly in log10-space.
    Appropriate for kinetic prefactors spanning several orders of magnitude.
    """
    low, high = bounds
    return 10.0 ** (
        np.log10(low)
        + u * (np.log10(high) - np.log10(low))
    )


def linear_sample(u, bounds):
    """
    Map u in [0, 1] linearly into the requested interval.
    """
    low, high = bounds
    return low + u * (high - low)


def build_diagnostic_points():
    """
    Legacy entry point retained only for compatibility with the run-mode logic.
    The final script is intended to run with RUN_DIAGNOSTIC_SWEEP = False.
    """
    raise RuntimeError(
        "Diagnostic mode is disabled in the final calibration script."
    )

def build_calibration_points():
    """
    Build the final 2-parameter Sobol calibration design.

    Free parameter 1
    ----------------
    Common precipitation prefactor:

        sf_k_intra = sf_k_GB = sf_k_prec

    sampled uniformly in log10-space within SF_K_PREC_BOUNDS.

    Free parameter 2
    ----------------
    Common CNT nucleation-barrier scaling:

        sf_dG_nucleation = sf_dG_nucleation_GB = sf_B_CNT

    sampled linearly within SF_B_CNT_BOUNDS.

    Fixed parameters
    ----------------
        sf_k_nucl       = 1
        sf_k_nucl_GB    = 1
        sf_dG_intra     = 1
        sf_dG_GB        = 1
        sf_k_res        = 1

    The first point is the explicit reference point identified by the
    diagnostic sweep, followed by 512 Sobol samples.
    """

    calibration_points = []

    # ------------------------------------------------------------
    # Explicit reference point from the final diagnostic
    # ------------------------------------------------------------
    reference = {
        "sf_k_intra": SF_K_PREC_REFERENCE,
        "sf_k_GB": SF_K_PREC_REFERENCE,

        "sf_k_nucl": SF_K_NUCL_FIXED,
        "sf_k_res": SF_K_RES_FIXED,

        "sf_dG_nucleation": SF_B_CNT_REFERENCE,
        "sf_dG_intra": SF_DG_INTRA_FIXED,
        "sf_dG_GB": SF_DG_GB_FIXED,

        "sf_dG_nucleation_gb": SF_B_CNT_REFERENCE,
        "sf_k_nucl_gb": SF_K_NUCL_GB_FIXED,

        "calibration_label": "diagnostic_reference",
    }

    calibration_points.append(reference)

    if RUN_BASELINE_ONLY:
        return calibration_points

    # ------------------------------------------------------------
    # Sobol sampling in 2 dimensions
    # ------------------------------------------------------------
    sampler = qmc.Sobol(
        d=2,
        scramble=True,
        seed=SOBOL_SEED
    )

    m = int(math.log2(N_SOBOL_SAMPLES))

    if 2**m != N_SOBOL_SAMPLES:
        raise ValueError(
            "N_SOBOL_SAMPLES must be a power of 2 "
            "when using random_base2()."
        )

    sobol_samples = sampler.random_base2(m=m)

    # ------------------------------------------------------------
    # Map Sobol coordinates into the final physical/calibration domain
    # ------------------------------------------------------------
    for sample in sobol_samples:

        sf_k_prec = log_sample(
            sample[0],
            SF_K_PREC_BOUNDS
        )

        sf_B_CNT = linear_sample(
            sample[1],
            SF_B_CNT_BOUNDS
        )

        params = {
            # Common precipitation prefactor
            "sf_k_intra": sf_k_prec,
            "sf_k_GB": sf_k_prec,

            # Fixed nucleation prefactors and re-solution
            "sf_k_nucl": SF_K_NUCL_FIXED,
            "sf_k_res": SF_K_RES_FIXED,

            # Common CNT barrier scale
            "sf_dG_nucleation": sf_B_CNT,

            # Fixed precipitation activation energies
            "sf_dG_intra": SF_DG_INTRA_FIXED,
            "sf_dG_GB": SF_DG_GB_FIXED,

            # Same common CNT barrier for GB nucleation
            "sf_dG_nucleation_gb": sf_B_CNT,
            "sf_k_nucl_gb": SF_K_NUCL_GB_FIXED,

            "calibration_label": "sobol",
        }

        calibration_points.append(params)

    return calibration_points


# MAIN CALIBRATION LOOP
def main():
    WORK.mkdir(exist_ok=True)

    sci_exe = find_sciantix_executable()
    print(f"Using SCIANTIX executable: {sci_exe}")
    print(f"Base folder: {BASE}")
    print()

    all_results = []
    skipped_results = []
    all_comparisons = []

    best = {
        "error": float("inf"),
        "params": None,
        "comparison": None,
    }

    if RUN_DIAGNOSTIC_SWEEP:
        calibration_points = build_diagnostic_points()
    else:
        calibration_points = build_calibration_points()

    total_parameter_sets = len(calibration_points)

    if RUN_DIAGNOSTIC_SWEEP:
        print("Diagnostic mode is not intended for this final calibration script.")

    elif RUN_BASELINE_ONLY:
        print("REFERENCE-ONLY MODE: running only the diagnostic reference point.")

    else:
        print("FINAL 2-PARAMETER SOBOL CALIBRATION")
        print(
            f"Calibration design: {N_SOBOL_SAMPLES} Sobol points "
            f"+ 1 explicit diagnostic reference = {total_parameter_sets} parameter sets."
        )
        print(
            "Free parameters: "
            "common precipitation prefactor and common CNT nucleation-barrier scale."
        )

    print(
        f"Each parameter set is evaluated at {len(conditions)} radial positions, "
        f"for a total of {total_parameter_sets * len(conditions)} SCIANTIX executions."
    )
    print()


    run_counter = 0

    for params in calibration_points:
        run_counter += 1

        diagnostic_label = params.get(
            "diagnostic_label",
            "baseline_or_calibration"
        )

        if RUN_DIAGNOSTIC_SWEEP:
            print()
            print(
                f"Running diagnostic set {run_counter}/{total_parameter_sets}: "
                f"{diagnostic_label}"
            )


        try:
            total_error, comparison_rows = evaluate_parameter_set(
                params,
                sci_exe
            )

        except Exception as e:
            skipped_results.append({
                "run": run_counter,
                **params,
                "exception_type": type(e).__name__,
                "exception_message": str(e),
            })

            print(
                f"[SKIP {run_counter}/{total_parameter_sets}] "
                f"sf_k_intra={params['sf_k_intra']:.3e}  "
                f"sf_k_GB={params['sf_k_GB']:.3e}  "
                f"sf_k_nucl={params['sf_k_nucl']:.3e}  "
                f"sf_k_res={params['sf_k_res']:.3e}  "
                f"sf_dG_nucl={params['sf_dG_nucleation']:.3e}  "
                f"sf_dG_intra={params['sf_dG_intra']:.3e}  "
                f"sf_dG_GB={params['sf_dG_GB']:.3e}  "
                f"sf_dG_nucl_GB={params['sf_dG_nucleation_gb']:.3e}  "
                f"sf_k_nucl_GB={params['sf_k_nucl_gb']:.3e}  "
                f"--> {type(e).__name__}: {e}"
            )
            continue

        # Save the four radial-position results for this diagnostic set
        for row in comparison_rows:
            all_comparisons.append({
                "run": run_counter,
                "diagnostic_label": diagnostic_label,
                **params,
                **row,
            })

        all_results.append({
            "run": run_counter,
            "diagnostic_label": diagnostic_label,
            "calibration_label": params.get("calibration_label", ""),
            "sf_k_prec_common": params["sf_k_intra"],
            "sf_B_CNT_common": params["sf_dG_nucleation"],
            "sf_k_intra": params["sf_k_intra"],
            "sf_k_GB": params["sf_k_GB"],
            "sf_k_nucl": params["sf_k_nucl"],
            "sf_k_res": params["sf_k_res"],
            "sf_dG_nucleation": params["sf_dG_nucleation"],
            "sf_dG_intra": params["sf_dG_intra"],
            "sf_dG_GB": params["sf_dG_GB"],
            "sf_dG_nucleation_GB": params["sf_dG_nucleation_gb"],
            "sf_k_nucl_GB": params["sf_k_nucl_gb"],
            "total_error": total_error,
        })

        print(
            f"[{run_counter}/{total_parameter_sets}] "
            f"sf_k_intra={params['sf_k_intra']:.3e}  "
            f"sf_k_GB={params['sf_k_GB']:.3e}  "
            f"sf_k_nucl={params['sf_k_nucl']:.3e}  "
            f"sf_k_res={params['sf_k_res']:.3e}  "
            f"sf_dG_nucl={params['sf_dG_nucleation']:.3e}  "
            f"sf_dG_intra={params['sf_dG_intra']:.3e}  "
            f"sf_dG_GB={params['sf_dG_GB']:.3e}  "
            f"sf_dG_nucl_GB={params['sf_dG_nucleation_gb']:.3e}  "
            f"sf_k_nucl_GB={params['sf_k_nucl_gb']:.3e}  "
            f"total_error={total_error:.6e}"
        )

        if total_error < best["error"]:
            best["error"] = total_error
            best["params"] = params.copy()
            best["comparison"] = comparison_rows

            print("  --> New best result")

    if skipped_results:
        skipped_df = pd.DataFrame(skipped_results)

        if RUN_DIAGNOSTIC_SWEEP:
            skipped_file = BASE / "diagnostic_unused_skipped.csv"
        else:
            skipped_file = BASE / "calibration_CNT_final_skipped.csv"

        skipped_df.to_csv(skipped_file, index=False)

    if len(all_results) == 0:
        print()
        print("No successful calibration run was completed.")
        if skipped_results:
            print(f"Saved skipped runs to: {BASE / 'calibration_CNT_final_skipped.csv'}")
        return

    results_df = pd.DataFrame(all_results)
    results_df = results_df.sort_values("total_error")

    if RUN_DIAGNOSTIC_SWEEP:
        results_file = BASE / "diagnostic_unused_results.csv"
        comparison_file = BASE / "diagnostic_unused_best_comparison.csv"
    else:
        results_file = BASE / "calibration_CNT_final_results.csv"
        comparison_file = BASE / "calibration_CNT_final_best_comparison.csv"

    results_df.to_csv(results_file, index=False)

    best_comparison_df = pd.DataFrame(best["comparison"])
    best_comparison_df.to_csv(comparison_file, index=False)

    if RUN_DIAGNOSTIC_SWEEP:
        diagnostic_comparison_df = pd.DataFrame(all_comparisons)
        diagnostic_comparison_df.to_csv(
            BASE / "diagnostic_unused_comparison_all.csv",
            index=False
        )

    print()
    print("============================================================")
    print("BEST RESULT")
    print("============================================================")
    print(f"sf_k_intra    = {best['params']['sf_k_intra']:.12e}")
    print(f"sf_k_GB       = {best['params']['sf_k_GB']:.12e}")
    print(f"sf_k_nucl     = {best['params']['sf_k_nucl']:.12e}")
    print(f"sf_k_res      = {best['params']['sf_k_res']:.12e}")
    print(f"sf_dG_nucleation = {best['params']['sf_dG_nucleation']:.12e}")
    print(f"sf_dG_intra   = {best['params']['sf_dG_intra']:.12e}")
    print(f"sf_dG_GB      = {best['params']['sf_dG_GB']:.12e}")
    print(f"sf_dG_nucleation_GB = {best['params']['sf_dG_nucleation_gb']:.12e}")
    print(f"sf_k_nucl_GB  = {best['params']['sf_k_nucl_gb']:.12e}")
    print(f"total_error   = {best['error']:.12e}")
    print()

    print("Saved:")
    print(f"  {results_file}")
    print(f"  {comparison_file}")

    if RUN_DIAGNOSTIC_SWEEP:
        print(f"  {BASE / 'diagnostic_energy_comparison_all.csv'}")

    if skipped_results:
        if RUN_DIAGNOSTIC_SWEEP:
            print(f"  {BASE / 'diagnostic_precipitation_skipped.csv'}")
        else:
            print(f"  {BASE / 'calibration_CNT_final_skipped.csv'}")


if __name__ == "__main__":
    main()