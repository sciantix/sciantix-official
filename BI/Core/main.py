from bi_mcmc import BIMCMC
from bi_da import BIDA
from dg_da import DGDA
from pp_bi_mcmc import PPBIMCMC
import os, tools
import numpy as np

def main():

    # case_name = 'test_diffusivity_case'
    case_name = 'test_Talip2014_1400K_b'
    params_key=np.array(['helium diffusivity pre exponential', 'helium diffusivity activation energy'])
    params_mean=np.array([0,1])
    params_std=np.array([1.526, 0.1])
    num_observed = 31 # red ooins
    num_calibration = 6

    # MCMC settings
    max_walk_steps = 100000
    num_walkers = 4 # > 2*number_of_scaling_factors
    bayI_mcmc = 'on'
    postP_mcmc = 'on'
    dataG_da = 'on'
    bayI_da = 'on'
    true_value = None
    num_per_param = np.array([300, 100]) # for data gen
    smooth_exp = False

    params_info = {params_key[i]:{'mean': params_mean[i],'std':params_std[i], 'num':num_per_param[i]} for i in range(len(params_key))}
    params_info = {key: params_info[key] for key in sorted(params_info)}

    params_key = np.sort(params_key)
    params_mean = np.array([value['mean'] for value in params_info.values()])
    params_std = np.array([value['std'] for value in params_info.values()])
    num_per_param = np.array([value['num'] for value in params_info.values()])

    case_folder_container = os.path.dirname(os.getcwd())
    case_folder = os.path.join(case_folder_container, case_name)

    fr_exp_info = tools.set_exp_data(case_folder, smooth = smooth_exp)
    time_max = np.max(fr_exp_info[:,0])
    time_observed = np.round(np.linspace(0, time_max, num_observed+1), 3)
    time_calibration = tools.set_calibration_time(time_observed, num_calibration, num_observed)
    data_observed = tools.interpolate(fr_exp_info[:,0], fr_exp_info[:,1], time_observed[1:])

    if bayI_mcmc == 'on':

        bimcmc = BIMCMC(
            case_folder = case_folder,
            params_key=params_key,
            params_mean=params_mean,
            params_std=params_std,
            num_observed = num_observed,
            num_calibration = num_calibration,
            max_num_iteration = max_walk_steps,
            num_walker = num_walkers
            )

        bimcmc.sampling(smooth_exp = False)

        
    if  postP_mcmc == 'on':
        ppbimcmc = PPBIMCMC(
            params_key = params_key,
            data_observed = data_observed,
            time_observed = time_observed,
            time_calibration = time_calibration,
            true_value = true_value
        )
        
    if dataG_da == 'on':
        dgda = DGDA(
            case_folder = case_folder,
            params_key=params_key,
            params_mean=params_mean,
            params_std=params_std,
            num_per_param = num_per_param,
        )


    if bayI_da == 'on':
        bida = BIDA(
            case_folder = case_folder,
            params_key=params_key,
            params_mean=params_mean,
            params_std=params_std,
            num_observed = num_observed,
            num_calibration = num_calibration,
            true_value = true_value,
        )
        bida.BI(smooth = smooth_exp)
        bida.plot(more_points = False)


if __name__ == "__main__":
    main()