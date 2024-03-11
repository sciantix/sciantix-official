import numpy as np
import tools, emcee, os, h5py, shutil
from scipy.optimize import minimize
import corner, ast
import matplotlib.pyplot as plt

class PPBIMCMC:
    """
    A class to do post processing for Bayesian Inference coupling with MCMC sampling.
    
    Args:
        *params_key* : the scaling factors name.
        *data_observed(nd array)* : the observed data from the exp that you used to do calibration.
        *time_observed(1d array)* : the corresponding time of the observed data.
        *time_calibration(1d array)* : the time of doing BI.
        *true_value(none or 1d array)* : the true value of the parameters 
    """

    def __init__(
        self,
        params_key,
        data_observed,
        time_observed,
        time_calibration,
        true_value = None,
    ):

        self.sfs_key = np.sort(params_key)
        self.true_value = true_value
        self.data_observed = data_observed

        calibrated_mean = np.zeros((len(time_calibration),len(self.sfs_key)))
        calibrated_low = np.zeros((len(time_calibration),len(self.sfs_key)))
        calibrated_up = np.zeros((len(time_calibration),len(self.sfs_key)))
        for i in range(len(time_calibration)):
            mcmc = emcee.backends.HDFBackend(f'backup{time_calibration[i]}.h5')
            tau = mcmc.get_autocorr_time()
            burnin = int(2 * np.max(tau))
            thin = int(0.5 * np.min(tau))
            samples = mcmc.get_chain(discard=burnin, flat=True, thin=thin)

            means = np.mean(samples, axis = 0)
            stds = np.std(samples,axis = 0)
            calibrated_low[i] = means - 3 * stds
            calibrated_up[i] = means + 3 * stds
            calibrated_mean[i] = means
            if not self.true_value == None:
                corner.corner(samples, labels = self.sfs_key, truths = self.true_value, quantiles = [0.05, 0.5, 0.95], show_titles = True, title_kwargs = {'fontsize':13})
            else:
                corner.corner(samples, labels = self.sfs_key, quantiles = [0.05, 0.5, 0.95], show_titles = True, title_kwargs = {'fontsize':13})
            plt.show()

        PP_folder = os.path.join(os.getcwd(), 'PP_BI_MCMC')
        if not os.path.exists(PP_folder):
            shutil.copytree(os.path.join(os.getcwd(), 'BI_MCMC'), 'PP_BI_MCMC')

        _ = np.sort(os.listdir(PP_folder))
        time_case_folder = []
        for i in range(len(_)):
            if _[i].startswith('from_0.0_to_'):
                time_case_folder.append(_[i])


        dict_calibrated_mean = [{key: value for key, value in zip(self.sfs_key, values)} for values in calibrated_mean]
        dict_calibrated_low = [{key: value for key, value in zip(self.sfs_key, values)} for values in calibrated_low]
        dict_claibrated_up = [{key: value for key, value in zip(self.sfs_key, values)} for values in calibrated_up]
        output_calibrated_mean = []
        output_calibrated_low = []
        output_calibrated_up = []
        for i in range(len(time_case_folder)):
            output_mean = tools.sciantix(os.path.join(PP_folder, time_case_folder[i]), dict_calibrated_mean[i], extraction_way = 0)[:,[0,2]]
            output_low = tools.sciantix(os.path.join(PP_folder, time_case_folder[i]), dict_calibrated_low[i], extraction_way = 0)[:,[0,2]]
            output_up = tools.sciantix(os.path.join(PP_folder, time_case_folder[i]), dict_claibrated_up[i], extraction_way = 0)[:,[0,2]]
            output_calibrated_mean.append(output_mean)
            output_calibrated_low.append(output_low)
            output_calibrated_up.append(output_up)


        color = plt.cm.viridis(np.linspace(0,1,len(time_calibration)))

        alpha = np.linspace(0.1, 0.5, len(time_calibration))
        for i in range(len(time_calibration)):
            plt.fill_between(output_calibrated_mean[i][:,0], output_calibrated_low[i][:,1],output_calibrated_up[i][:,1],color = color[i], alpha = alpha[i])
            plt.plot(output_calibrated_mean[i][:,0], output_calibrated_mean[i][:,1],c = color[i], linestyle = 'dashdot', label = f"Calibrated@{np.round(time_calibration[i],3)}")
            
            plt.axvline(time_calibration[i], c = color[i], linestyle = 'dashdot')


        # plt.plot(output_exp[:,0], output_exp[:,1], label = 'Reference', c = 'r', label = 'Real')
        plt.scatter(time_observed[1:], self.data_observed, marker = '*', color = 'r', label = 'Observed')
        plt.xlabel('time / h')
        plt.ylabel('helium fraction release /')
        plt.legend()
        plt.title('Helium fraction release with mean diffusivity calibrated at different time')
        plt.show()

        for i in range(len(self.sfs_key)):
            plt.plot(time_calibration, calibrated_mean[:,i], label = f'{self.sfs_key[i]}', c = 'g')
            plt.plot(time_calibration, calibrated_low[:,i], label = 'low_bound', c = 'g', linestyle = 'dashdot')
            plt.plot(time_calibration, calibrated_up[:,i],label = 'up_bound', c = 'g', linestyle = 'dashdot')
            if not self.true_value == None:
                plt.plot([0, time_calibration[-1]], [self.true_value[i], self.true_value[i]], label = 'Real', c = 'r' )

            plt.legend()
            plt.xlabel('time / h')
            plt.ylabel('scaling_factor')
            plt.show()
