import numpy as np
import tools, emcee, os, h5py, warnings
from scipy.optimize import minimize
from scipy.stats import truncnorm

class BIMCMC:
    """
    Bayesian Inference with Markov Chain Monte Carlo sampling.
    A class to do Bayesian Inference coupling with MCMC sampling for scaling factors.

    Args:
        *case_folder (string)* : the path of the experimental case that you want to utilize as observations.
        *params_key (1d array)* : the scaling factors name you want to sample.
        *params_mean (1d array)* : the prior mean of the scaling factors.
        *params_std (1d array)* : the prior standard deviation of the scaling factors.
        *num_observed (int scaler)* : the number of the observations that you want to extract from the exp data.
        *num_calibration(int scaler)* : the number of the calibrations that you want to do.
        *max_num_iteration (int scaler)* : the maximum iterations for mcmc sampler.
        *num_walker (int scaler)* : the walker number of the affine invariant ensembler.
    methods:
        *set_multiplicative_factor(mean, std)* : active the multiplicative model bias { y = exp(fm * ym) * ym + error }
        *sampling(smooth_exp = False)* : sampling scaling factors value from the desired posterior.

    properties:
        *time_observed(1d array)* : the time of the observation.
        *time_calibration(1d array)* : the time of doing BI.
        *data_observed(1d array)* : the observed data from the exp.
    """
    def __init__(
        self,
        case_folder,
        params_key,
        params_mean,
        params_std,
        num_observed,
        num_calibration,
        max_num_iteration,
        num_walker
    ):
        params_info = {params_key[i]:{'mean': params_mean[i],'std':params_std[i]} for i in range(len(params_key))}
        if num_calibration > num_observed:
            num_calibration = num_observed
            warnings.warn("Caution, num_calibration is larger num_observed.")
        elif num_calibration == None:
            num_calibration = num_observed
        self.case_folder = case_folder
        self.sciantix_required_files_path = tools.sciantix_required_files_path(case_folder)
        self.params_info = {key: params_info[key] for key in sorted(params_info)}
        self.num_observed = num_observed
        self.num_calibration = num_calibration
        self.max_num_iteration = max_num_iteration
        self.num_walker = num_walker
        self.Calibration_folder = tools.setup_directory('BI_MCMC')
        self.sfs_key = np.sort(params_key)

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

    def set_multiplicative_factor(self, mean, std):
        self.params_info['fm'] = {'mean': mean, 'std': std}

    def sampling(self, smooth_exp = False):
        fr_exp_info = self._set_exp_data(smooth_exp)
        # print(fr_exp_info)
        time_max = np.max(fr_exp_info[:,0])
        time_observed = np.round(np.linspace(0, time_max, self.num_observed+1), 3)
        time_calibration = self._set_calibration_time(time_observed)

        initial_values = np.array([value['mean'] for value in self.params_info.values()])
        covariance = np.diag([value['std'] ** 2 for value in self.params_info.values()])
        nll = lambda * args: -np.exp(self._log_likelihood(*args))

        for i in range(self.num_calibration):
            time_counted = time_observed[time_observed <= time_calibration[i]]
            # time_counted = time_counted[1:]
            fr_real = tools.interpolate(fr_exp_info[:,0], fr_exp_info[:,1], time_counted[1:])
            fr_std = tools.interpolate(fr_exp_info[:,0], fr_exp_info[:,2], time_counted[1:])
            sciantix_folder_path = tools.independent_sciantix_folder(
                self.Calibration_folder, 
                self.sciantix_required_files_path,
                time_counted,
                prediction = False,
                original_history = True)
            soln = minimize(nll, initial_values, args = (fr_real, fr_std, sciantix_folder_path, time_counted[1:]))
            pos = soln.x + 1e-4 * np.random.randn(self.num_walker, len(self.params_info))

            nwalkers, ndim = pos.shape
            file_name = f'backup{time_calibration[i]}.h5'
            backend = emcee.backends.HDFBackend(file_name)
            backend.reset(nwalkers, ndim)

            sampler = emcee.EnsembleSampler(nwalkers, ndim, self._log_prob, args = (initial_values, covariance, fr_real, fr_std, sciantix_folder_path, time_counted[1:]), backend = backend)
            
            index = 0
            autucorr = np.empty(self.max_num_iteration)
            old_tau = np.inf
            for sample in sampler.sample(pos, iterations = self.max_num_iteration, progress = True):
                if sampler.iteration % 100:
                    continue
                tau = sampler.get_autocorr_time(tol = 0)
                autucorr[index] = np.mean(tau)
                index +=1

                converged = np.all(tau * 100 < sampler.iteration)
                converged &= np.all(np.abs(old_tau - tau)/tau < 0.01)
                if converged:
                    break
                old_tau = tau

            tau = sampler.get_autocorr_time()
            burnin = int(2 * np.max(tau))
            thin = int(0.5 * np.min(tau))
            
        self.data_observed = fr_real
        self.data_std = fr_std
        self.time_calibration = time_calibration
        self.time_observed = time_observed
        

    def _log_likelihood(self, theta, y_real, y_std, sciantix_folder_path, time_observed):
        
        if len(self.params_info) == len(self.sfs_key):
            sfs_value = theta
            fm = 0
        else:
            sfs_value = theta[:-1]
            fm = theta[-1]
        sfs = {key:value for key, value in zip(self.sfs_key, sfs_value)}
        y_model = tools.sciantix(sciantix_folder_path, sfs, extraction_way = time_observed)[:,2]
        sigma2 = y_std ** 2
        return -0.5 * np.sum((y_real - np.exp(fm*y_model) * y_model) ** 2 / sigma2 + np.log(2*np.pi*sigma2))
    
    def _log_prob(self, theta, mean, variance, y_real, y_std, sciantix_folder_path, time_observed):
        lp = tools.log_tg_prior(theta, mean, variance)
        # lp = self.log_uniform_prior(theta, mean, variance)
        if not np.isfinite(lp):
            return -np.inf
        return lp + self._log_likelihood(theta, y_real, y_std, sciantix_folder_path, time_observed)

    # @property
    # def time_calibration(self):
    #     return self.time_calibration
    
    # @property
    # def time_observed(self):
    #     return self.time_observed

    # @property
    # def data_observed(self):
    #     return self.data_observed
    
    # @property
    # def data_std(self):
    #     return self.data_std


# case_name = 'test_Talip2014_1400K_b'
# case_folder_container = os.path.dirname(os.getcwd())
# case_folder = os.path.join(case_folder_container, case_name)


# cali = BI_MCMC(
#     case_folder = case_folder,
#     params_key=np.array(['helium diffusivity pre exponential', 'helium diffusivity activation energy']),
#     params_mean=np.array([0,1]),
#     params_std=np.array([1.526, 0.1]),
#     num_observed = 31,
#     num_calibration =1, 
#     max_num_iteration = 100000,
#     num_walker = 4
#     )

# cali.sampling()
