import numpy as np
import tools, os, shutil, json, corner, random
import seaborn as sns 
import matplotlib.pyplot as plt

class DGDA:
    """
    Data Generation for Deterministic Analysis.
    A class to generate the sciantix output with the input of scaling factors uniformly sampled within their range.

    Args:
        *case_folder (string)* : the path of the experimental case of reference.
        *params_key (1d array)* : the scaling factors name you want to sample.
        *params_mean (1d array)* : the prior mean of the scaling factors.
        *params_std (1d array)* : the prior standard deviation of the scaling factors.
        *num_per_param (1d array)* : the number of the sampling for each parameters.

    """
    def __init__(
        self,
        case_folder,
        params_key,
        params_mean,
        params_std,
        num_per_param
    ):

        #### set the parameters that need to be calibrated ####
        params_info = {params_key[i]:{'mean': params_mean[i],'std':params_std[i],'num':num_per_param[i]} for i in range(len(params_key))}
        params_info = {key: params_info[key] for key in sorted(params_info)}

        params_key = np.sort(params_key)
        params_mean = np.array([value['mean'] for value in params_info.values()])
        params_std = np.array([value['std'] for value in params_info.values()])
        num_per_param = np.array([value['num'] for value in params_info.values()])
        
        params_bound = np.array([[value['mean']-3 * value['std'], value['mean'] + 3 * value['std']] for value in params_info.values()])
        params_value = []
        index = []
        for i, key in enumerate(params_key):
            if 'pre exponential' in key:
                params_bound[i] = np.exp(params_bound[i])
                index.append(i)
            sf = np.linspace(params_bound[i, 0], params_bound[i, 1], num_per_param[i])
            params_value.append(sf)

        grid = np.meshgrid(*params_value, indexing = 'ij')
        sfs = np.stack(grid, axis = -1).reshape(-1, len(params_value))
        if len(index) != 0:
            sfs[:,index] = np.log(sfs[:,index])


        DA_folder = tools.setup_directory('DA')
        sciantix_required_files_path = tools.sciantix_required_files_path(case_folder)
        for file in sciantix_required_files_path:
            shutil.copy(file, DA_folder)

        with tools.change_directory(DA_folder, os.getcwd()):
            np.savetxt('input_da.txt', sfs)
            cwd = os.getcwd()
            outputs = []
            for i in range(len(sfs)):
                params = {key:value for key, value in zip(params_key, sfs[i])}
                output = tools.sciantix(cwd, params, extraction_way = 0)[:,2]
                outputs.append(output)

            outputs = np.array(outputs)
            np.savetxt('output_da.txt', outputs)


