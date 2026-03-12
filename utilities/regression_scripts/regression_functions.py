"""
This is a python script to execute the regression (running the validation database) of SCIANTIX.

@author G. Zullo
"""

import os
import numpy as np
import matplotlib.pyplot as plt
import shutil

def import_data(filename):
    """
    Imports a tab-delimited .txt file into a NumPy ndarray.
    
    Parameters
    ----------
    filename : str
        The path to the .txt file to import.
    
    Returns
    -------
    ndarray
        The data from the file as a NumPy array.
    """
    try:
        data = np.genfromtxt(filename, dtype='str', delimiter='\t')
    except Exception as e:
        raise RuntimeError(f"Error importing data from {filename}: {e}")
    return data

def findSciantixVariablePosition(output, variable_name):
    """
    Finds the column index of a specified variable in the ndarray.
    
    Parameters
    ----------
    output : ndarray
        The data array where variable names are searched.
    variable_name : str
        The name of the variable to find.
    
    Returns
    -------
    int
        The column index of the variable.
    
    Raises
    ------
    ValueError
        If the variable name is not found.
    """
    i, j = np.where(output == variable_name)
    if j.size == 0:
        raise ValueError(f"Variable '{variable_name}' not found.")
    return int(j[0])

def plot(x, y, title='Plot', xlabel='X-axis', ylabel='Y-axis'):
    """
    Plots data with labels on the x and y axes.
    
    Parameters
    ----------
    x : ndarray
        1D array with the x-axis data; the first element is the label.
    y : ndarray
        1D array with the y-axis data; the first element is the label.
    title : str, optional
        The title of the plot (default is 'Plot').
    xlabel : str, optional
        The label for the x-axis (default is 'X-axis').
    ylabel : str, optional
        The label for the y-axis (default is 'Y-axis').
    
    Returns
    -------
    None
    """
    # Setting the plot
    fig, ax = plt.subplots()
    ax.plot(x[1:].astype(float), y[1:].astype(float), color='blue')
    ax.set_title(title)
    ax.set_xlabel(xlabel)
    ax.set_ylabel(ylabel)
    plt.show()

def are_files_equal(file1, file2):
    """
    Compares two files to check if they are identical.
    
    Parameters
    ----------
    file1 : str
        The path to the first file.
    file2 : str
        The path to the second file.
    
    Returns
    -------
    bool
        True if files are identical, False otherwise.
    
    Raises
    ------
    FileNotFoundError
        If either of the files does not exist.
    """
    if not os.path.exists(file1):
        raise FileNotFoundError(f"{file1} not found.")
    if not os.path.exists(file2):
        raise FileNotFoundError(f"{file2} not found.")
    
    with open(file1, 'r') as f1, open(file2, 'r') as f2:
        return f1.read() == f2.read()

def sciantix_dictionary(file):
    """
    Parses a file and maps variables to their corresponding labels.
    
    Parameters
    ----------
    file : str
        The path to the file to parse.
    
    Returns
    -------
    dict
        A dictionary where keys are variable names and values are the corresponding data arrays.
    
    Raises
    ------
    ValueError
        If a variable label is not found in the data.
    """
    data = import_data(file)
    
    variable_labels = {
        "t": "Time (h)",
        "T": "Temperature (K)",
        "frate": "Fission rate (fiss / m3 s)",
        "sigma": "Hydrostatic stress (MPa)",
        "bu": "Burnup (MWd/kgUO2)",
        "N": "Intergranular bubble concentration (bub/m2)",
        "n": "Intergranular atoms per bubble (at/bub)",
        "v": "Intergranular vacancies per bubble (vac/bub)",
        "rad": "Intergranular bubble radius (m)",
        "A": "Intergranular bubble area (m2)",
        "vol": "Intergranular bubble volume (m3)",
        "fc": "Intergranular fractional coverage (/)",
        "fcsat": "Intergranular saturation fractional coverage (/)",
        "f": "Intergranular fractional intactness (/)",
        "fv": "Intergranular vented fraction (/)",
        "pv": "Intergranular venting probability (/)",
        "xe_p": "Xe produced (at/m3)",
        "xe_pHBS": "Xe produced in HBS (at/m3)",
        "xe_ig": "Xe in grain (at/m3)",
        "xe_igHBS": "Xe in grain HBS (at/m3)",
        "xe_igb": "Xe in intragranular bubbles (at/m3)",
        "xe_igs": "Xe in intragranular solution (at/m3)",
        "xe_gb": "Xe at grain boundary (at/m3)",
        "xe_r": "Xe released (at/m3)",
        "fgr": "Fission gas release (/)",
        "alpha": "Restructured volume fraction (/)",
        "sv": "Intergranular S/V (1/m)",
        "swe_gb": "Intergranular gas swelling (/)",
        "swe_igb": "Intragranular gas bubble swelling (/)",
        "swe_igs": "Intragranular gas solution swelling (/)",
        "a": "Grain radius (m)",
        "fima": "FIMA (%)",
    }

    sd = {}
    for var_name, var_label in variable_labels.items():
        try:
            var_index = findSciantixVariablePosition(data, var_label)
            sd[var_name] = data[1:, var_index].astype(float)
        except ValueError as e:
            sd[var_name] = np.zeros_like(data[1:, 0].astype(float))
            print(f"Variable '{var_label}' not found in the data: {e}")

    return sd


def check_result(number_of_tests_failed):
    """Verify the test results by comparing output with gold standard."""
    if are_files_equal('output.txt', 'output_gold.txt'):
        print("Test passed!\n")
    else:
        print("Test failed!\n")
        number_of_tests_failed += 1
    return number_of_tests_failed

def check_output(file):
    """Verify the existence of the output files and read their data."""
    try:
        data = import_data("output.txt")
    except FileNotFoundError:
        print("output.txt not found in", file)
        data = np.zeros((1, 1))

    try:
        data_gold = import_data("output_gold.txt")
    except FileNotFoundError:
        print("output_gold.txt not found in", file)
        data_gold = np.ones((1, 1))

    return data, data_gold

def do_sciantix():
    """Execute SCIANTIX and handle input/output files."""
    shutil.copy("../input_settings.txt", os.getcwd())
    shutil.copy("../input_scaling_factors.txt", os.getcwd())
    shutil.copy("../sciantix.x", os.getcwd())
    os.system("./sciantix.x")
    os.remove("sciantix.x")
    os.remove("execution.txt")
    os.remove("input_check.txt")

def do_sciantix_only():
    """Execute SCIANTIX and handle input/output files."""
    shutil.copy("../sciantix.x", os.getcwd())
    os.system("./sciantix.x")
    os.remove("sciantix.x")
    os.remove("execution.txt")
    os.remove("input_check.txt")

def do_gold():
    """Replace the existing gold file with the latest output."""
    if os.path.exists('output.txt'):
        if os.path.exists('output_gold.txt'):
            os.remove('output_gold.txt')
        shutil.copy('output.txt', 'output_gold.txt')        

    else:
        print("output.txt not found for golding")
        
		