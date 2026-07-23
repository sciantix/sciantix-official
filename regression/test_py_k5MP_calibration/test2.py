from pathlib import Path
import shutil
import subprocess
import math
import numpy as np
import pandas as pd



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
OMEGA_5MP = 1.06651e-29



# range sensati per i coefficienti
SF_K_INTRA_VALUES = np.logspace(-0.30, 0.30, 7)
SF_K_GB_VALUES    = np.logspace(-0.30, 0.30, 7)
SF_K_NUCL_VALUES  = np.logspace(-0.30, 0.30, 7)
SF_K_RES_VALUES   = np.array([1.0])
SF_DG_NUCLEATION_VALUES = np.linspace(0.94, 1.04, 9)


# WEIGHTED CALIBRATION SETTINGS
# If True, each error contribution is multiplied by:
#     w_final = w_radial * w_observable
# If False, all contributions are treated with unit weight.
USE_WEIGHTED_OBJECTIVE = True

# Radial reliability weights
# r000  -> 0R: clear image, but more heterogeneous/double population
# r030  -> 0.30R: less reliable image/microstructural interpretation
# r056  -> 0.56R: reference reliable position
# r075  -> 0.75R: reference reliable position
RADIAL_WEIGHTS = {
    "r000": 0.75,
    "r030": 0.50,
    "r056": 1.00,
    "r075": 1.00,
}

# Observable reliability weights
# Fractions are weighted less than the intragranular mean radius
# because they are more sensitive to thresholding, segmentation,
# field-of-view selection and 2D-to-3D comparison.
OBSERVABLE_WEIGHTS = {
    "intra_radius": 1.00,
    "intra_fraction": 0.60,
    "inter_fraction": 0.75,
}

# ============================================================
# HIERARCHICAL / LEXICOGRAPHIC SELECTION SETTINGS
# ============================================================
# If True, the code does not select the best parameter set only
# by the global weighted error.
#
# Logic:
#   1) minimize the PRIMARY error first;
#   2) accept only solutions with primary_error within
#      PRIMARY_ERROR_TOLERANCE of the minimum primary_error;
#   3) among those acceptable solutions, choose the one with the
#      lowest full weighted error.
#
# This prevents the optimizer from accepting a small improvement
# of low-reliability targets if it worsens the most reliable targets.
USE_HIERARCHICAL_SELECTION = True

# Allowed degradation with respect to the best primary error.
# 0.02 = within +2% of the minimum primary error.
PRIMARY_ERROR_TOLERANCE = 0.02

# Primary targets: most reliable observables to preserve first.
# Here: intragranular mean radius at 0.56R and 0.75R.
# Available target names:
#   "intra_fraction", "intra_radius", "inter_fraction"
PRIMARY_TARGETS = {
    "r056": ["intra_radius"],
    "r075": ["intra_radius"],
}

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


# EXPERIMENTAL CONDITIONS
conditions = [
    {
        "name": "r000",
        "r_over_R": 0.00,
        "burnup_exp": BURNUP_EXP,
        "exp_intra": EXP_INTRA / "exp_intra_r000.txt",
        "exp_inter": EXP_INTER / "exp_inter_r000.txt",
    },
    {
        "name": "r030",
        "r_over_R": 0.30,
        "burnup_exp": BURNUP_EXP,
        "exp_intra": EXP_INTRA / "exp_intra_r030.txt",
        "exp_inter": EXP_INTER / "exp_inter_r030.txt",
    },
    {
        "name": "r056",
        "r_over_R": 0.56,
        "burnup_exp": BURNUP_EXP,
        "exp_intra": EXP_INTRA / "exp_intra_r056.txt",
        "exp_inter": EXP_INTER / "exp_inter_r056.txt",
    },
    {
        "name": "r075",
        "r_over_R": 0.75,
        "burnup_exp": BURNUP_EXP,
        "exp_intra": EXP_INTRA / "exp_intra_r075.txt",
        "exp_inter": EXP_INTER / "exp_inter_r075.txt",
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
    "{{SF_K_INTRA}}": f"{params['sf_k_intra']:.12e}",
    "{{SF_K_GB}}": f"{params['sf_k_GB']:.12e}",
    "{{SF_K_NUCL}}": f"{params['sf_k_nucl']:.12e}",
    "{{SF_K_RES}}": f"{params['sf_k_res']:.12e}",
    "{{SF_DG_NUCLEATION}}": f"{params['sf_dG_nucleation']:.12e}",
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


def read_sciantix_output(run_dir, burnup_exp):
    output_file = run_dir / OUTPUT_NAME
    require_file(output_file)

    df = read_table_with_tabs(output_file)
    idx = select_output_row(df, burnup_exp)

    Cm_prec_intra = float(df.loc[idx, COL_CM_PREC_INTRA])
    Cm_prec_GB = float(df.loc[idx, COL_CM_PREC_GB])

    N_intra = float(df.loc[idx, COL_INTRA_5MP_CONC])
    atoms_per_5MP = float(df.loc[idx, COL_ATOMS_PER_5MP])

    # Experimental Inclusions is area fraction.
    # Model comparison uses precipitated volume fraction.
    # Stereological approximation: area fraction ≈ volume fraction.
    f_intra_model = Cm_prec_intra * OMEGA_5MP
    f_GB_model = Cm_prec_GB * OMEGA_5MP

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

    selected_burnup = None
    if BURNUP_COLUMN is not None:
        selected_burnup = float(df.loc[idx, BURNUP_COLUMN])

    return {
        "selected_burnup": selected_burnup,
        "f_intra": f_intra_model,
        "R_intra": R_intra_model,
        "f_GB": f_GB_model,
        "Cm_prec_intra": Cm_prec_intra,
        "Cm_prec_GB": Cm_prec_GB,
        "N_intra": N_intra,
        "atoms_per_5MP": atoms_per_5MP,
    }


# ERROR FUNCTION
def relative_error_squared(model_value, exp_value):
    if exp_value is None or exp_value <= 0.0:
        return 0.0

    return ((model_value - exp_value) / exp_value) ** 2


def compute_condition_error(model, exp_intra, exp_inter, condition):
    err_intra_fraction = relative_error_squared(
        model["f_intra"],
        exp_intra["inclusions"]
    )

    err_intra_radius = relative_error_squared(
        model["R_intra"],
        exp_intra["radius"]
    )

    err_inter_fraction = relative_error_squared(
        model["f_GB"],
        exp_inter["inclusions"]
    )

    if USE_WEIGHTED_OBJECTIVE:
        radial_weight = RADIAL_WEIGHTS.get(condition["name"], 1.0)

        w_intra_fraction = radial_weight * OBSERVABLE_WEIGHTS["intra_fraction"]
        w_intra_radius = radial_weight * OBSERVABLE_WEIGHTS["intra_radius"]
        w_inter_fraction = radial_weight * OBSERVABLE_WEIGHTS["inter_fraction"]
    else:
        radial_weight = 1.0
        w_intra_fraction = 1.0
        w_intra_radius = 1.0
        w_inter_fraction = 1.0

    weighted_err_intra_fraction = w_intra_fraction * err_intra_fraction
    weighted_err_intra_radius = w_intra_radius * err_intra_radius
    weighted_err_inter_fraction = w_inter_fraction * err_inter_fraction

    total = (
        weighted_err_intra_fraction
        + weighted_err_intra_radius
        + weighted_err_inter_fraction
    )

    return total, {
        "err_intra_fraction": err_intra_fraction,
        "err_intra_radius": err_intra_radius,
        "err_inter_fraction": err_inter_fraction,

        "radial_weight": radial_weight,
        "w_intra_fraction": w_intra_fraction,
        "w_intra_radius": w_intra_radius,
        "w_inter_fraction": w_inter_fraction,

        "weighted_err_intra_fraction": weighted_err_intra_fraction,
        "weighted_err_intra_radius": weighted_err_intra_radius,
        "weighted_err_inter_fraction": weighted_err_inter_fraction,
    }


def compute_primary_error_contribution(error_terms, condition):
    """
    Error contribution used for hierarchical selection.

    The primary error is computed only from the targets listed in
    PRIMARY_TARGETS. By default this means the intragranular mean radius
    at r056 and r075.

    The primary error uses the non-weighted relative squared errors of
    the selected targets. In the present default case this is equivalent
    to the weighted value, because r056/r075 intra_radius have final
    weight equal to 1.
    """
    selected_targets = PRIMARY_TARGETS.get(condition["name"], [])

    primary_error = 0.0

    if "intra_fraction" in selected_targets:
        primary_error += error_terms["err_intra_fraction"]

    if "intra_radius" in selected_targets:
        primary_error += error_terms["err_intra_radius"]

    if "inter_fraction" in selected_targets:
        primary_error += error_terms["err_inter_fraction"]

    return primary_error


# PARAMETER EVALUATION
def evaluate_parameter_set(params, sci_exe):
    total_error = 0.0
    primary_error = 0.0
    comparison_rows = []

    for condition in conditions:
        exp_intra = read_exp_file(condition["exp_intra"])
        exp_inter = read_exp_file(condition["exp_inter"])

        run_dir = prepare_run_folder(condition, params)
        run_sciantix(run_dir, sci_exe)

        model = read_sciantix_output(run_dir, condition["burnup_exp"])

        condition_error, error_terms = compute_condition_error(
            model,
            exp_intra,
            exp_inter,
            condition
        )

        condition_primary_error = compute_primary_error_contribution(
            error_terms,
            condition
        )

        total_error += condition_error
        primary_error += condition_primary_error

        comparison_rows.append({
            "case": condition["name"],
            "r_over_R": condition["r_over_R"],
            "burnup_exp": condition["burnup_exp"],
            "burnup_model_selected": model["selected_burnup"],

            "exp_intra_inclusions": exp_intra["inclusions"],
            "model_intra_fraction": model["f_intra"],

            "exp_intra_radius_m": exp_intra["radius"],
            "model_intra_radius_m": model["R_intra"],

            "exp_inter_inclusions": exp_inter["inclusions"],
            "model_GB_fraction": model["f_GB"],

            "err_intra_fraction": error_terms["err_intra_fraction"],
            "err_intra_radius": error_terms["err_intra_radius"],
            "err_inter_fraction": error_terms["err_inter_fraction"],

            "radial_weight": error_terms["radial_weight"],
            "w_intra_fraction": error_terms["w_intra_fraction"],
            "w_intra_radius": error_terms["w_intra_radius"],
            "w_inter_fraction": error_terms["w_inter_fraction"],

            "weighted_err_intra_fraction": error_terms["weighted_err_intra_fraction"],
            "weighted_err_intra_radius": error_terms["weighted_err_intra_radius"],
            "weighted_err_inter_fraction": error_terms["weighted_err_inter_fraction"],
            "condition_error": condition_error,
            "primary_error_contribution": condition_primary_error,

            "Cm_prec_intra": model["Cm_prec_intra"],
            "Cm_prec_GB": model["Cm_prec_GB"],
            "N_intra": model["N_intra"],
            "atoms_per_5MP": model["atoms_per_5MP"],
        })

    return total_error, primary_error, comparison_rows


# MAIN CALIBRATION LOOP
def main():
    WORK.mkdir(exist_ok=True)

    sci_exe = find_sciantix_executable()
    print(f"Using SCIANTIX executable: {sci_exe}")
    print(f"Base folder: {BASE}")
    if USE_WEIGHTED_OBJECTIVE:
        print("Objective function: weighted")
        print(f"Radial weights: {RADIAL_WEIGHTS}")
        print(f"Observable weights: {OBSERVABLE_WEIGHTS}")
    else:
        print("Objective function: equal-weight")

    if USE_HIERARCHICAL_SELECTION:
        print("Selection strategy: hierarchical")
        print(f"Primary targets: {PRIMARY_TARGETS}")
        print(f"Primary tolerance: {PRIMARY_ERROR_TOLERANCE:.3f}")
    else:
        print("Selection strategy: minimum total weighted error")
    print()

    all_results = []
    all_comparisons = {}

    run_counter = 0

    for sf_k_intra in SF_K_INTRA_VALUES:
        for sf_k_GB in SF_K_GB_VALUES:
            for sf_k_nucl in SF_K_NUCL_VALUES:
                for sf_k_res in SF_K_RES_VALUES:
                    for sf_dG_nucleation in SF_DG_NUCLEATION_VALUES:

                        params = {
                            "sf_k_intra": float(sf_k_intra),
                            "sf_k_GB": float(sf_k_GB),
                            "sf_k_nucl": float(sf_k_nucl),
                            "sf_k_res": float(sf_k_res),
                            "sf_dG_nucleation": float(sf_dG_nucleation),
                        }

                        run_counter += 1

                        try:
                            total_error, primary_error, comparison_rows = evaluate_parameter_set(
                                params,
                                sci_exe
                            )

                        except Exception as e:
                            print(
                                f"[SKIP] "
                                f"sf_k_intra={sf_k_intra:.3e}  "
                                f"sf_k_GB={sf_k_GB:.3e}  "
                                f"sf_k_nucl={sf_k_nucl:.3e}  "
                                f"sf_k_res={sf_k_res:.3e}  "
                                f"sf_dG={sf_dG_nucleation:.3e}  "
                            )
                            continue

                        all_results.append({
                            "run_id": run_counter,
                            "sf_k_intra": sf_k_intra,
                            "sf_k_GB": sf_k_GB,
                            "sf_k_nucl": sf_k_nucl,
                            "sf_k_res": sf_k_res,
                            "sf_dG_nucleation": sf_dG_nucleation,
                            "primary_error": primary_error,
                            "total_error": total_error,
                        })

                        all_comparisons[run_counter] = comparison_rows

                        print(
                            f"[{run_counter}] "
                            f"sf_k_intra={sf_k_intra:.3e}  "
                            f"sf_k_GB={sf_k_GB:.3e}  "
                            f"sf_k_nucl={sf_k_nucl:.3e}  "
                            f"sf_k_res={sf_k_res:.3e}  "
                            f"sf_dG={sf_dG_nucleation:.3e}  "
                            f"primary={primary_error:.6e}  "
                            f"weighted={total_error:.6e}"
                        )

    if len(all_results) == 0:
        raise RuntimeError("No successful calibration run was completed.")

    results_df = pd.DataFrame(all_results)

    if USE_HIERARCHICAL_SELECTION:
        primary_min = results_df["primary_error"].min()
        primary_limit = primary_min * (1.0 + PRIMARY_ERROR_TOLERANCE)

        eligible_df = results_df[
            results_df["primary_error"] <= primary_limit
        ].copy()

        # Among all candidates that preserve the primary targets,
        # choose the one with the lowest full weighted error.
        best_row = eligible_df.sort_values("total_error").iloc[0]

        eligible_df = eligible_df.sort_values("total_error")
        eligible_df.to_csv(BASE / "hierarchical_eligible_results.csv", index=False)

    else:
        primary_min = None
        primary_limit = None
        eligible_df = None
        best_row = results_df.sort_values("total_error").iloc[0]

    results_df = results_df.sort_values("total_error")
    results_df.to_csv(BASE / "calibration_results.csv", index=False)

    best_run_id = int(best_row["run_id"])
    best_comparison_df = pd.DataFrame(all_comparisons[best_run_id])
    best_comparison_df.to_csv(BASE / "best_comparison.csv", index=False)

    print()
    print("============================================================")
    print("BEST RESULT")
    print("============================================================")

    if USE_HIERARCHICAL_SELECTION:
        print("Selection strategy: hierarchical")
        print(f"minimum primary_error = {primary_min:.12e}")
        print(f"allowed primary_error = {primary_limit:.12e}")
        print(f"eligible candidates   = {len(eligible_df)}")
    else:
        print("Selection strategy: minimum total weighted error")

    print(f"run_id        = {best_run_id}")
    print(f"sf_k_intra    = {best_row['sf_k_intra']:.12e}")
    print(f"sf_k_GB       = {best_row['sf_k_GB']:.12e}")
    print(f"sf_k_nucl     = {best_row['sf_k_nucl']:.12e}")
    print(f"sf_k_res      = {best_row['sf_k_res']:.12e}")
    print(f"sf_dG_nucleation = {best_row['sf_dG_nucleation']:.12e}")
    print(f"primary_error = {best_row['primary_error']:.12e}")
    print(f"total_error   = {best_row['total_error']:.12e}")
    print()

    print("Saved:")
    print(f"  {BASE / 'calibration_results.csv'}")
    print(f"  {BASE / 'best_comparison.csv'}")
    if USE_HIERARCHICAL_SELECTION:
        print(f"  {BASE / 'hierarchical_eligible_results.csv'}")


if __name__ == "__main__":
    main()