import numpy as np
import tools, emcee, os, h5py
from scipy.optimize import minimize
# from multiprocessing import Pool
import multiprocessing
import matplotlib.pyplot as plt

class CalibrationMCMC:
    def __init__(
        self,
        case_folder,
        params_key,
        params_mean,
        params_std,
        points_time,
        max_walk_steps,
        multiplicative_factor,
        # points_assign2process
        ):

        params_info = {key:[mean, std] for key, mean, std in zip(params_key, params_mean, params_std)}
        self.params_info = {key:params_info[key] for key in sorted(params_info)}
        self.case_folder = case_folder
        self.points_time = points_time
        keys = np.array([key for key in self.params_info.keys()])
        tools.setup_directory('Calibration')

        fr_exp_info = tools.exp_data_process(case_folder)
        path_input_initial_conditions = os.path.join(case_folder, 'input_initial_conditions.txt')
        path_input_settings = os.path.join(case_folder,'input_settings.txt')
        path_input_scaling_factor = os.path.join(case_folder, 'input_scaling_factors.txt')
        path_input_history = os.path.join(case_folder, 'input_history.txt')
        _ = os.path.dirname(os.getcwd())
        _ = os.path.dirname(_)
        _ = os.path.dirname(_)
        path_sciantix = os.path.join(_,'bin', 'sciantix.x')
        required_files_path = [path_input_initial_conditions, path_input_history, path_input_scaling_factor, path_input_settings, path_sciantix]
        
        nll = lambda *args: -np.exp(self._log_likelihood(*args))
        
        initial_values = np.array([value[0] for value in self.params_info.values()])
        covariance = np.diag([value[1] ** 2 for value in self.params_info.values()])

        if multiplicative_factor == True:
            initial_values = np.zeros(len(params_key)+1)
            initial_values[:-1] = np.array([value[0] for value in self.params_info.values()])
            initial_values[-1] = 0.01
            original_covariance = covariance
            covariance = np.zeros((len(params_key)+1, len(params_key)+1))
            for j in range(len(params_key)):
                covariance[j,j] = original_covariance[j,j]
            covariance[-1,-1] = 0.005
        samples = []

        for i in range(20,len(points_time)):
        # for i in range(1+np.sum(poins_assigned2process[:-1]),np.sum(poins_assigned2process)+1):
            sciantix_folder_path = tools.independent_sciantix_folder('Calibration', required_files_path, points_time[:i+1], prediction = False)
            interpolated_data = self.interpolate(fr_exp_info, points_time[1:i+1])
            fr_real = interpolated_data[:,1]
            fr_std = interpolated_data[:,2]
            soln = minimize(nll, initial_values, args = (keys, fr_real, fr_std,points_time[1:i+1], sciantix_folder_path, multiplicative_factor))
            
            pos = soln.x + 1e-4 * np.random.randn(10, len(initial_values))
            nwalkers, ndim = pos.shape

            file_name = f'backup{points_time[i]}.h5'
            backend = emcee.backends.HDFBackend(file_name)
            backend.reset(nwalkers, ndim)
            # covariance = np.diag([value[1] ** 2 for value in self.params_info.values()])
            
            np.set_printoptions(threshold=np.inf)

            sampler = emcee.EnsembleSampler(nwalkers, ndim, self.log_posterior, args = (initial_values, covariance, keys,fr_real, fr_std,points_time[1:i+1], sciantix_folder_path, multiplicative_factor), backend = backend)
            
            index = 0
            autucorr = np.empty(max_walk_steps)
            old_tau = np.inf
            iii = 0
            for sample in sampler.sample(pos, iterations = max_walk_steps, progress = True):
                # if sampler.iteration % 100:
                #     continue
                # tau = sampler.get_autocorr_time(tol = 0)
                # autucorr[index] = np.mean(tau)
                # index +=1

                # converged = np.all(tau * 100 < sampler.iteration)
                # converged &= np.all(np.abs(old_tau - tau)/tau < 0.01)
                # if converged:
                #     break
                # old_tau = tau
                iii = iii+1
            # sampler.run_mcmc(pos, walk_steps, progress = True)
                with open(f'MCMC_samples_{iii}.txt', 'w') as file:
                    for k, array in enumerate(samples):
                        file.write(np.array2string(array, separator=', ') + "\n\n")
                # tau = sampler.get_autocorr_time()
                # burnin = int(2 * np.max(tau))
                # thin = int(0.5 * np.min(tau))
                flat_samples = sampler.get_chain(flat = True)
                lpg_propb_samples = sampler.get_log_prob( flat = True)
                log_prior_samples = sampler.get_blobs(flat = True)
                samples.append(flat_samples)
                
                plt.scatter(flat_samples[:,0], flat_samples[:,1])
                plt.show()
                

            


    @staticmethod
    def _log_prior(theta, mean, covariance):
        n = len(theta)
        diff = theta - mean
        return -0.5*(n * np.log(2 * np.pi) + np.log(np.linalg.det(covariance)) + np.dot(diff, np.linalg.solve(covariance, diff)))
    @staticmethod
    def _log_likelihood(theta, params_key, y_real, y_std, points_time, sciantix_folder_path, multiplicative_factor = True):
        params_value = theta
        fm = 0
        if multiplicative_factor == True:
            params_value = theta[:-1]
            fm = theta[-1]
        params = {key:value for key, value in zip(params_key, params_value)}
        y_model = tools.sciantix(sciantix_folder_path, params, extraction_way = points_time)[:,2]
        sigma2 = y_std ** 2
        return -0.5 * np.sum((y_real - np.exp(fm*y_model) * y_model) ** 2 / sigma2 + np.log(2*np.pi*sigma2))
    
    def log_posterior(self, theta, mean, covariance, params_key, y_real, y_std, points_time, sciantix_folder_path, multiplicative_factor):
        lp = self._log_prior(theta, mean, covariance)
        if not np.isfinite(lp):
            return -np.inf
        return lp + self._log_likelihood(theta, params_key, y_real, y_std, points_time, sciantix_folder_path, multiplicative_factor)
    @staticmethod
    def interpolate(data, points_time):

        interpolated_data = np.zeros((len(points_time),data.shape[1]))

        for i in range(len(points_time)):
            # print(data.shape)
            clostest_point = tools.find_closest_points(data, points_time[i])

            interpolated_data[i] = tools.linear_interpolate(*clostest_point, points_time[i])
        return interpolated_data


processs = []
case_name = 'test_Talip2014_1600K'
case_folder_container = os.path.dirname(os.getcwd())
case_folder = os.path.join(case_folder_container, case_name)


cali = CalibrationMCMC(
    case_folder = case_folder,
    params_key=np.array(['helium diffusivity pre exponential', 'helium diffusivity activation energy']),
    params_mean=np.array([0,1]),
    params_std=np.array([1.526, 0.1]),
    points_time = np.linspace(0,3.6,21),
    max_walk_steps = 10,
    multiplicative_factor = False)

