import numpy as np
import tools, emcee, os, h5py
from scipy.optimize import minimize
# from multiprocessing import Pool
import multiprocessing
import tensorflow as tf 
import joblib
from sklearn.preprocessing import MinMaxScaler
from tensorflow.keras.models import load_model
from scipy.interpolate import interp1d

def interpolate(x, y, x_toInter):

    idx = np.searchsorted(x, x_toInter) - 1
    idx = np.clip(idx,0, len(x) - 2)
    dy = y[idx+1] - y[idx]
    dt = x[idx+1] - x[idx]
    t_diff = x_toInter -x[idx]
    return y[idx] + (dy/dt) * t_diff


def custom_loss(y_true, y_pred):
    # Ensure values are within [0, 1]
    bounded_loss = tf.reduce_mean(tf.square(tf.clip_by_value(y_pred, 0, 1) - y_pred))
    
    # Penalize decreases in successive elements (encourage monotonic increase)
    diff = y_pred[:, 1:] - y_pred[:, :-1]
    decrease_loss = tf.reduce_mean(tf.square(tf.minimum(diff, 0)))
    
    # Standard MSE loss
    mse_loss = tf.keras.losses.mean_squared_error(y_true, y_pred)
    
    return mse_loss + bounded_loss + decrease_loss





class CalibrationMCMC:
    def __init__(
        self,
        case_folder,
        ann_model,
        scaler_input,
        scaler_output,
        params_mean,
        params_std,
        points_time,
        max_walk_steps,
        multiplicative_factor,
        # points_assign2process
        ):

        # tools.setup_directory('Calibration')
        time_model = np.genfromtxt('time_model.txt')
        model = load_model('Talip_1600_2.h5', custom_objects = {'custom_loss': custom_loss})
        
        fr_exp_info = tools.exp_data_process(case_folder)
        # print(fr_exp_info.shape)

        nll = lambda *args: -np.exp(self._log_likelihood(*args))
        original_std = params_std
        original_mean = params_mean

        if multiplicative_factor == True:
            covariance = np.zeros((len(original_mean)+1,len(original_mean)+1))
            theta = np.hstack((params_mean, np.array(0.01)))
            original_covariance = np.diag(original_std**2)
            for j in range(len(original_mean)):
                covariance[j,j] = original_covariance[j,j]
            covariance[-1,-1] = 0.005
        else:
            theta = params_mean
            covariance = np.diag(original_std**2)

        theta_initial = theta
        samples = []

        for i in range(20,len(points_time)):
        # for i in range(1+np.sum(poins_assigned2process[:-1]),np.sum(poins_assigned2process)+1):
            # interpolated_data = self.interpolate(fr_exp_info[:,0], fr_exp_info[:,1], points_time[1:i+1])
            # fr_real = interpolated_data[:,1]
            # fr_std = interpolated_data[:,2]
            fr_real = interpolate(fr_exp_info[:,0], fr_exp_info[:,1], points_time[1:i+1])
            fr_std = interpolate(fr_exp_info[:,0], fr_exp_info[:,2], points_time[1:i+1])
            soln = minimize(nll, theta_initial, args = (fr_real, fr_std,points_time[1:i+1],time_model, model, scaler_input, scaler_output, multiplicative_factor))
            
            pos = soln.x + 1e-4 * np.random.randn(10, len(theta_initial))
            nwalkers, ndim = pos.shape

            file_name = f'backup{points_time[i]}.h5'
            backend = emcee.backends.HDFBackend(file_name)
            backend.reset(nwalkers, ndim)
            # covariance = np.diag([value[1] ** 2 for value in self.params_info.values()])
            
            np.set_printoptions(threshold=np.inf)

            sampler = emcee.EnsembleSampler(nwalkers, ndim, self.log_posterior, args = (theta_initial, covariance, fr_real, fr_std, points_time[1:i+1],time_model, model, scaler_input, scaler_output, multiplicative_factor), backend = backend)
            
            index = 0
            autucorr = np.empty(max_walk_steps)
            old_tau = np.inf
            for sample in sampler.sample(pos, iterations = max_walk_steps, progress = True):
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
            
            # sampler.run_mcmc(pos, walk_steps, progress = True)
            

            tau = sampler.get_autocorr_time()
            burnin = int(2 * np.max(tau))
            thin = int(0.5 * np.min(tau))
            flat_samples = sampler.get_chain(discard = burnin, thin = thin, flat = True)
            lpg_propb_samples = sampler.get_log_prob(discard = burnin, thin = thin, flat = True)
            log_prior_samples = sampler.get_blobs(discard = burnin, thin = thin, flat = True)
            flat_samples = scaler_input.inverse_transform(flat_samples)
            samples.append(flat_samples)
            with open('MCMC_samples.txt', 'w') as file:
                for k, array in enumerate(samples):
                    file.write(np.array2string(array, separator=', ') + "\n\n")
            
            print(tau)


    @staticmethod
    def _log_prior(theta, mean, covariance):
        n = len(theta)
        diff = theta - mean
        return -0.5*(n * np.log(2 * np.pi) + np.log(np.linalg.det(covariance)) + np.dot(diff, np.linalg.solve(covariance, diff)))
    @staticmethod
    def _log_likelihood(theta, y_real, y_std, points_time, time_model, model, scaler_input, scaler_output, multiplicative_factor):
        if multiplicative_factor == True:
            sfs = theta[:-1]
            sfs = np.exp(sfs)
            fm = theta[-1]
        else:
            sfs = np.exp(theta)
            fm = 0
        
        sfs = scaler_input.transform(np.array([sfs]))
        sfs = sfs.reshape((1, 1, 2))
        y_model = model.predict(sfs)
        y_model = scaler_output.inverse_transform(y_model)

        y_interpolated = interpolate(time_model, y_model[0], points_time)
        sigma2 = y_std ** 2
        return -0.5 * np.sum((y_real - np.exp(fm*y_interpolated) * y_interpolated) ** 2 / sigma2 + np.log(2*np.pi*sigma2))
    
    def log_posterior(self, theta, mean, covariance, y_real, y_std, points_time, time_model, model, scaler_input, scaler_output, multiplicative_factor):
        lp = self._log_prior(theta, mean, covariance)
        if not np.isfinite(lp):
            return -np.inf
        return lp + self._log_likelihood(theta, y_real, y_std, points_time, time_model, model, scaler_input, scaler_output, multiplicative_factor)
    @staticmethod
    def custom_loss(y_true, y_pred):
        # Ensure values are within [0, 1]
        bounded_loss = tf.reduce_mean(tf.square(tf.clip_by_value(y_pred, 0, 1) - y_pred))
        
        # Penalize decreases in successive elements (encourage monotonic increase)
        diff = y_pred[:, 1:] - y_pred[:, :-1]
        decrease_loss = tf.reduce_mean(tf.square(tf.minimum(diff, 0)))
        
        # Standard MSE loss
        mse_loss = tf.keras.losses.mean_squared_error(y_true, y_pred)
        
        return mse_loss + bounded_loss + decrease_loss




# tools.setup_directory('Calibration')
processs = []
case_name = 'test_Talip2014_1600K'
case_folder_container = os.path.dirname(os.getcwd())
case_folder = os.path.join(case_folder_container, case_name)
# params_key=np.array(['helium diffusivity pre exponential', 'helium diffusivity activation energy'])
# params_mean=np.array([0,1])
# params_std=np.array([1.526, 0.1])
# points_time = np.linspace(0,3.6,21)
# max_walk_steps = 100000

# poins_assigned2process = [8,7,5]


# p1 = multiprocessing.Process(target = CalibrationMCMC, args=[case_folder, params_key, params_mean, params_std, points_time, max_walk_steps, np.array([0,8])])
# p2 = multiprocessing.Process(target = CalibrationMCMC, args=[case_folder, params_key, params_mean, params_std, points_time, max_walk_steps, np.array([8,7])])
# p3 = multiprocessing.Process(target = CalibrationMCMC, args=[case_folder, params_key, params_mean, params_std, points_time, max_walk_steps, np.array([8,7,5])])

# p1.start()
# p2.start()
# p3.start()

# p1.join()
# p2.join()
# p3.join()


model = load_model('Talip_1600_2.h5', custom_objects = {'custom_loss': custom_loss})
scaler_input = joblib.load('scaler_input.pkl')
scaler_output = joblib.load('scaler_output.pkl')
time_model = np.loadtxt('time_model.txt')
# print(time_model.shape)


cali = CalibrationMCMC(
    case_folder = case_folder,
    ann_model = model,
    scaler_input = scaler_input,
    scaler_output = scaler_output,
    params_mean=np.array([0,1]),
    params_std=np.array([1.526, 0.1]),
    points_time = np.linspace(0,time_model[-1],21),
    max_walk_steps = 100000,
    multiplicative_factor = True)
