"""

This is a python script to execute the perform the correlation update in sciantix.

@author G. Nicodemo

"""

""" ------------------- Import requiered depedencies ------------------- """

import os
import shutil
import numpy as np
import matplotlib.pyplot as plt
from matplotlib import cm
from matplotlib import rcParams

import Correlations
import GP_functions
import GP_util

""" ------------------- Main part -------------------"""
def main():
    # Stock the directory path of the current file
    wpath = os.path.dirname(os.path.realpath(__file__))
    os.chdir(wpath) # Change the working directory to the path stored in 'wpath'

    # Load settings
    settings = GP_util.read_one_parameter("settings.txt", "settings_check.txt")

    " -------- Xe Diffusion Coefficient -------- " 
    # Move to the data folder
    folder_name = "XeDiffusionCoefficient/XeDiffusionCoefficientData.txt"
    data_path = os.path.join(wpath, folder_name)

    # Load the dataset
    try:
        dataXeDiffusionCoefficient = GP_functions.load_txt_to_matrix(data_path)
    except Exception as e:
        print(f"Error loading dataset: {e}")
        return
    
    # Make the regression
    if settings[0] == 1:
        from XeDiffusionCoefficient import XeDiffusionCoefficient
        
        options_file_path = os.path.abspath(os.path.join(wpath, 'XeDiffusionCoefficient', 'XeDiffusionCoefficientOptions.txt'))
        options = GP_util.load_settings(options_file_path)
        
        TT_plot = np.array(np.linspace(options[0],options[1],options[2])).reshape(-1,1)
        F_plot  = np.array(np.linspace(options[3],options[4],options[5])).reshape(-1,1)
        evaluation_grid = np.column_stack((TT_plot,F_plot))
        
        # Conversion to boolean
        options[9] = bool(options[9])
        
        updated_correlationXe, std_sXe = XeDiffusionCoefficient.GPregression_XeDiffusionCoefficient(data = dataXeDiffusionCoefficient, correlation = Correlations.Turnbull_D, evaluation_range = evaluation_grid, 
                                                                                                l = options[6], sigma = options[7], scaling_factor = options[8], DoPlot = options[9], update_setting = options[10])
        
        updated_correlationXe = np.transpose(updated_correlationXe)
        
        # Write the results into an output file
        GP_util.write_matrix_to_txt(updated_correlationXe,os.path.join(wpath, "Results"), "UpdatedCorrelation_XeDiffusionCoefficient.txt")
        GP_util.write_matrix_to_txt(std_sXe,              os.path.join(wpath, "Results"), "StandardDeviation_XeDiffusionCoefficient.txt")
        # Move plots to the proper folder
        GP_util.move_all_images(wpath, os.path.join(wpath, "XeDiffusionCoefficient"))
        
    " -------- MOX Melting Temperature -------- " 
    # Move to the data folder
    folder_name = "MOXMeltingTemperature/MOXMeltingTemperatureData.txt"
    data_path = os.path.join(wpath, folder_name)

    # Load the dataset
    try:
        dataMoxMeltingTemperature = GP_functions.load_txt_to_matrix(data_path)
    except Exception as e:
        print(f"Error loading dataset: {e}")
        return
    
    # Make the regression
    if settings[1] == 1:
        from MOXMeltingTemperature import MOXMeltingTemperature
        
        options_file_path = os.path.abspath(os.path.join(wpath, 'MOXMeltingTemperature', 'MOXMeltingTemperatureOptions.txt'))
        options = GP_util.load_settings(options_file_path)
        
        X_plot = np.array(np.linspace(options[0],options[1],options[2])).reshape(-1,1)
        PU_plot  = np.array(np.linspace(options[3],options[4],options[5])).reshape(-1,1)
        evaluation_grid = np.column_stack((X_plot,PU_plot))
        
        # Conversion to boolean
        options[9] = bool(options[9])
        
        updated_correlation_MOXTmelt, std_s_MOXTmelt = MOXMeltingTemperature.GPregression_MOXMeltingTemperature(data = dataMoxMeltingTemperature, correlation = Correlations.Magni_MOX_Tmelt, evaluation_range = evaluation_grid, 
                                                                                                l = options[6], sigma = options[7], scaling_factor = options[8], DoPlot = options[9], update_setting = options[10], Np = options[11],
                                                                                                FixedSeed = options[12])
        
        # Write the results into an output file
        GP_util.write_matrix_to_txt(updated_correlation_MOXTmelt,os.path.join(wpath, "Results"), "UpdatedCorrelation_MOXMeltingTemperature.txt")
        GP_util.write_matrix_to_txt(std_s_MOXTmelt,              os.path.join(wpath, "Results"), "StandardDeviation_MOXMeltingTemperature.txt")
        # Move plots to the proper folder
        GP_util.move_all_images(wpath, os.path.join(wpath, "MOXMeltingTemperature"))
        
    return 
    
# If the script is being run as the main program, call the 'main' function.
if __name__ == "__main__":
    main()
