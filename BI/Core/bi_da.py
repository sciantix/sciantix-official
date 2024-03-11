import numpy as np
import tools, os, shutil, json, corner, random, copy
import seaborn as sns 
import matplotlib.pyplot as plt
from scipy.stats import truncnorm



class BIDA:
    """
    Bayesian Inference with Deterministic Analysis method.
    A class to perform Bayesian Inference with the data generated from deterministic scaling factors.

    Args:
        *case_folder (string)* : the path of the experimental case that you want to utilize as observations.
        *params_key (1d array)* : the scaling factors name you want to sample.
        *params_mean (1d array)* : the prior mean of the scaling factors.
        *params_std (1d array)* : the prior standard deviation of the scaling factors.
        *num_observed (int scaler)* : the number of the observations that you want to extract from the exp data.
        *num_calibration(int scaler)* : the number of the calibrations that you want to do.
        *true_value(none or 1d array)* : the true value of the parameters.
    Methods:
        *BI(smooth = False)* :
    """

    

    def __init__(
        self,
        case_folder,
        params_key,
        params_mean,
        params_std,
        num_observed,
        num_calibration,
        true_value = None,
    ):
        params_info = {params_key[i]:{'mean': params_mean[i],'std':params_std[i]} for i in range(len(params_key))}
        self.params_info = {key: params_info[key] for key in sorted(params_info)}
        
        if num_calibration > num_observed:
            num_calibration = num_observed
            warnings.warn("Caution, num_calibration is larger num_observed.")
        elif num_calibration == None:
            num_calibration = num_observed        
        
        self.sfs_key = np.sort(params_key)
        self.true_value = true_value
        self.covariance_matrix = np.diag([value['std'] ** 2 for value in self.params_info.values()])
        self.params_mean = np.array([value['mean'] for value in self.params_info.values()])
        self.num_observed = num_observed
        self.num_calibration = num_calibration
        self.case_folder = case_folder

    def _set_exp_data(self, smooth = True):
        if smooth == True:
            fr_exp_info = tools.exp_data_process(self.case_folder)
        else:
            with tools.change_directory(self.case_folder, os.getcwd()):
                fr_exp = np.genfromtxt("Talip2014_release_data.txt",dtype = 'float',delimiter='\t')
                fr_exp = fr_exp[fr_exp[:,0].argsort()]
                fr_std = tools.dynamic_std(fr_exp[:,1], window_size = 20)
                fr_exp_info = np.hstack((fr_exp, fr_std.reshape((-1, 1))))
        return fr_exp_info

    def _set_calibration_time(self, time_observed):
        if self.num_calibration - 1 == 0:
            time_calibration = np.array([time_observed[-1]])
        else:
            points_per_calibration = self.num_observed//self.num_calibration
            # extra_points = (self.time_observed - 1)%(self.num_calibration)
            time_calibration = np.zeros((self.num_calibration))
            for i in range(self.num_calibration - 1):
                time_calibration[i] = time_observed[int(points_per_calibration * (i+1))]
            time_calibration[-1] = time_observed[-1]
        return time_calibration

    def BI(self, smooth = False):
        DA_folder = os.path.join(os.getcwd(), 'DA')
        time_model = tools.get_selected_variables_value_from_output(["Time (h)"], os.path.join(DA_folder, 'output.txt'))

        with tools.change_directory(DA_folder, os.getcwd()):
            output_da = np.loadtxt('output_da.txt')
            sfs_da = np.loadtxt('input_da.txt').reshape((len(output_da), len(self.params_info)))

        fr_exp_info = self._set_exp_data(smooth = smooth)
        time_max = np.max(fr_exp_info[:,0])
        time_observed = np.round(np.linspace(0, time_max, self.num_observed+1), 3)
        time_calibration = self._set_calibration_time(time_observed)
        index = np.array([np.where(time_observed == time)[0][0] for time in time_calibration]) - 1

        #########
        fr_real = tools.interpolate(fr_exp_info[:,0], fr_exp_info[:,1], time_observed[1:])
        fr_std = tools.interpolate(fr_exp_info[:,0], fr_exp_info[:,2], time_observed[1:])
        fr_model = []
        for i, fr in enumerate(output_da):
            fr_inter = tools.interpolate(time_model.flatten(), fr, time_observed[1:])
            fr_model.append(fr_inter)
        fr_model = np.array(fr_model)


        log_probability = np.zeros((sfs_da.shape[0], len(time_observed)-1))
        # log_probability_calibration = np.zeros((sfs_da.shape[0], len(time_calibration)))
        posteriors = []
        posteriors_calibration = []
        for j in range(len(time_observed) - 1):
            if j == 0:
                means = copy.deepcopy(self.params_mean)
                covariance_matrix = copy.deepcopy(self.covariance_matrix)
                for i in range(sfs_da.shape[0]):
                    log_probability[i, 0] = self._log_prob(sfs_da[i], means , covariance_matrix, fr_real[0], fr_std[0], fr_model[i, 0])
                posterior = np.exp(log_probability[:,0])/np.sum(np.exp(log_probability[:,0]))
                posteriors.append(posterior)
            else:
                prior = posteriors[-1]
                log_prior = np.zeros_like(prior)
                for k in range(len(prior)):
                    if prior[k] == 0:
                        log_prior[k] = -np.inf
                    else:
                        log_prior[k] = np.log(prior[k])
                for i in range(sfs_da.shape[0]):
                    # fr_model = tools.interpolate(time_model.flatten(), output_da[i], time_counted[1:])
                    log_lh = self._log_likelihood(fr_real[j], fr_std[j], fr_model[i, j])
                    log_probability[i,j] = log_prior[i] + log_lh
                posterior= np.exp(log_probability[:,j])/np.sum(np.exp(log_probability[:,j]))
                posteriors.append(posterior)
        for i in range(len(time_calibration)):
            posteriors_calibration.append(posteriors[index[i]])
        
        # posteriors_calibration = np.array(posteriors_calibration)
        # log_probability = np.zeros((sfs_da.shape[0], len(time_calibration)))
        # posteriors = []

        # for j in range(len(time_calibration)):
        #     time_counted = time_observed[time_observed <= time_calibration[j]]
            
        #     fr_real = tools.interpolate(fr_exp_info[:,0], fr_exp_info[:,1], time_counted[1:])
        #     fr_std = tools.interpolate(fr_exp_info[:,0], fr_exp_info[:,2], time_counted[1:])
        #     if j == 0:
        #         for i in range(sfs_da.shape[0]):
        #             fr_model = tools.interpolate(time_model.flatten(), output_da[i], time_counted[1:])
        #             log_probability[i, j] = self._log_prob(sfs_da[i], self.params_mean, self.covariance_matrix, fr_real, fr_std, fr_model)
        #             posterior = np.exp(log_probability[:,j])/np.sum(np.exp(log_probability[:,j]))
        #             posteriors.append(posterior)
        #     else:
        #         prior = posteriors[-1]
        #         log_prior = np.zeros_like(prior)
        #         for k in range(len(prior)):
        #             if prior[k] == 0:
        #                 log_prior[k] = -np.inf
        #             else:
        #                 log_prior[k] = np.log(prior[k])
        #         for i in range(sfs_da.shape[0]):
        #             fr_model = tools.interpolate(time_model.flatten(), output_da[i], time_counted[1:])
        #             log_lh = self._log_likelihood(fr_real, fr_std, fr_model)
        #             log_probability[i,j] = log_prior[i] + log_lh
        #         posterior= np.exp(log_probability[:,j])/np.sum(np.exp(log_probability[:,j]))
        #         posteriors.append(posterior)
        
        self.posteriors_calibration = np.array(posteriors_calibration)
        self.sfs_da = sfs_da
        self.DA_folder = DA_folder
        self.time_calibration = time_calibration
        self.time_observed = time_observed
        self.data_observed = fr_real
    
    def plot(self, more_points = False):
        index = []
        for i, key in enumerate(self.sfs_key):
            if 'pre exponential' in key:
                index.append(i)

        calibrated_mean = np.zeros((self.num_calibration, len(self.sfs_key)))
        calibrated_low = np.zeros((self.num_calibration, len(self.sfs_key)))
        calibrated_up = np.zeros((self.num_calibration, len(self.sfs_key)))

        for i in range(self.num_calibration):
            samples = self.sampler(self.sfs_da, self.posteriors_calibration[i], 10000)
            stds = np.std(samples, axis = 0)
            calibrated_mean[i] = np.mean(samples, axis = 0)
            calibrated_low[i] = calibrated_mean[i] - 3* stds
            calibrated_up[i] = calibrated_mean[i] + 3* stds
            if self.true_value != None:
                fig = corner.corner(samples, labels = self.sfs_key,quantiles = [0.05, 0.5, 0.95],truths = self.true_value, show_titles = True, title_kwargs = {'fontsize':13})
            else:
                fig = corner.corner(samples, labels = self.sfs_key,quantiles = [0.05, 0.5, 0.95], show_titles = True, title_kwargs = {'fontsize':13})
            plt.show()

        # if len(index) != 0:
        #     calibrated_mean[:,index] = np.log(calibrated_mean[:,index])
        #     calibrated_low[:,index] = np.log(calibrated_low[:,index])
        #     calibrated_up[:,index] = np.log(calibrated_up[:,index])


        dict_calibrated_mean = [{key: value for key, value in zip(self.sfs_key, values)} for values in calibrated_mean]
        dict_calibrated_low = [{key: value for key, value in zip(self.sfs_key, values)} for values in calibrated_low]
        dict_claibrated_up = [{key: value for key, value in zip(self.sfs_key, values)} for values in calibrated_up]
        output_calibrated_mean = []
        output_calibrated_low = []
        output_calibrated_up = []
        for i in range(self.num_calibration):
            output_mean = tools.sciantix(self.DA_folder, dict_calibrated_mean[i], extraction_way = 0)[:,[0,2]]
            output_low = tools.sciantix(self.DA_folder, dict_calibrated_low[i], extraction_way = 0)[:,[0,2]]
            output_up = tools.sciantix(self.DA_folder, dict_claibrated_up[i], extraction_way = 0)[:,[0,2]]
            output_calibrated_mean.append(output_mean)
            output_calibrated_low.append(output_low)
            output_calibrated_up.append(output_up)


        color = plt.cm.viridis(np.linspace(0.1, 0.5,len(self.time_calibration)))
        alpha = np.linspace(0.1, 0.5, len(self.time_calibration))
        for i in range(len(self.time_calibration)):
            plt.fill_between(output_calibrated_mean[i][:,0], output_calibrated_low[i][:,1],output_calibrated_up[i][:,1],color = color[i], alpha = alpha[i])
            plt.plot(output_calibrated_mean[i][:,0], output_calibrated_mean[i][:,1],c = color[i], linestyle = 'dashdot', label = f"Calibrated@{np.round(self.time_calibration[i],3)}")
            
            plt.axvline(self.time_calibration[i], c = color[i], linestyle = 'dashdot')

        plt.scatter(self.time_observed[1:], self.data_observed, marker = '*', color = 'r', label = 'Observed')
        plt.xlabel('time / h')
        plt.ylabel('helium fraction release /')
        plt.legend()
        plt.title('Helium fraction release with mean diffusivity calibrated at different time')
        plt.show()

        for i in range(len(self.sfs_key)):
            plt.plot(self.time_calibration, calibrated_mean[:,i], label = f'{self.sfs_key[i]}', c = 'g')
            plt.plot(self.time_calibration, calibrated_low[:,i], label = 'low_bound', c = 'g', linestyle = 'dashdot')
            plt.plot(self.time_calibration, calibrated_up[:,i],label = 'up_bound', c = 'g', linestyle = 'dashdot')
            if not self.true_value == None:
                plt.plot([0, self.time_calibration[-1]], [self.true_value[i], self.true_value[i]], label = 'Real', c = 'r' )

            plt.legend()
            plt.xlabel('time / h')
            plt.ylabel('scaling_factor')
            plt.show()



    @staticmethod
    def _log_likelihood(y_real, y_std, y_model):
        sigma2 = y_std ** 2
        return -0.5 * np.sum((y_real - y_model) ** 2 / sigma2 + np.log(2*np.pi*sigma2))

    def _log_prob(self, theta, mean, covariance_matrix, y_real, y_std, y_model):
        lp = tools.log_tg_prior(theta, mean, covariance_matrix)
        if not np.isfinite(lp):
            return -np.inf
        return lp + self._log_likelihood(y_real, y_std, y_model)

    @staticmethod
    def sampler(sfs, prob, num_samples, more_points = False):
        """
        sfs: [act, pre]
        """
        prob_normalized = prob/prob.sum()
        sampled_indices = np.random.choice(len(sfs), size = num_samples, p = prob_normalized)
        sampled_points = sfs[sampled_indices]
        if more_points == True:
            if sfs.shape[1] == 2:
                h_width = abs(sfs[0,0] - sfs[1,0])
                v_width = abs(sfs[0,0] - sfs[0,1])
                for i in range(len(sampled_points)):
                    sampled_points[i,0] = sampled_points[i, 0] + random.uniform(-h_width/2, h_width/2)
                    sampled_points[i,1] = sampled_points[i, 1] + random.uniform(-v_width/2, v_width/2)
            else:
                h_width = abs(sfs[0, 0] - sfs[1,0])
                for i in range(len(sampled_points)):
                    sampled_points[i,0] = sampled_points[i, 0] + random.uniform(-h_width/2, h_width/2)

        return sampled_points
