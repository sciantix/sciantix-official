"""

This is a python script to execute the perform the correlation update in sciantix.

@author G. Nicodemo

"""

""" ------------------- Import requiered depedencies ------------------- """

import os
import numpy as np
import matplotlib.pyplot as plt
from matplotlib import cm
from matplotlib import rcParams

import Correlations
import GP_functions
import GP_util

" ------------------- Main part -------------------"
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
        
        updated_correlation, std_s = XeDiffusionCoefficient.GPregression_XeDiffusionCoefficient(data = dataXeDiffusionCoefficient, correlation = Correlations.Turnbull_D, evaluation_range = evaluation_grid, 
                                                                                                l = options[6], sigma = options[7], scaling_factor = options[8], DoPlot = options[9], update_setting = options[10])
        
        updated_correlation = np.transpose(updated_correlation)
        
        # Write the results into an output file
        GP_util.write_matrix_to_txt(updated_correlation,os.path.join(wpath, "Results"), "UpdatedCorrelation_XeDiffusionCoefficient.txt")
        GP_util.write_matrix_to_txt(std_s,              os.path.join(wpath, "Results"), "StandardDeviation_XeDiffusionCoefficient.txt")
        # Move plots to the proper folder
        GP_util.move_file(wpath, os.path.join(wpath, "XeDiffusionCoefficient"), "Weight function.png")
        GP_util.move_file(wpath, os.path.join(wpath, "XeDiffusionCoefficient"), "Contour of the weight function.png")
        GP_util.move_file(wpath, os.path.join(wpath, "XeDiffusionCoefficient"), "2D Diffusion Coefficient Correlation Update.png")
        
    return updated_correlation, std_s
    
# If the script is being run as the main program, call the 'main' function.
if __name__ == "__main__":
    main()
