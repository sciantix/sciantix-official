import numpy as np
from scipy.stats import norm, multivariate_normal, uniform, gaussian_kde
import os, shutil, emcee
from itertools import product
from data_generation import DataGeneration
from scipy.optimize import minimize
from user_model import UserModel
import matplotlib.pyplot as plt 
import corner


model = UserModel(
    case_name='test_Talip2014_1600K',
    # params=np.array(['helium diffusivity pre exponential', 'henry constant pre exponential']),
    # params_initial_values=np.array([0,0]),
    # params_stds=np.array([1.526,1.417])

    # params=np.array(['helium diffusivity pre exponential']),
    # params_initial_values=np.array([0]),
    # params_stds=np.array([1.526])

    params=np.array(['helium diffusivity pre exponential', 'helium diffusivity activation energy']),
    params_initial_values=np.array([0,1]),
    params_stds=np.array([1.526, 0.1])
) 
params_info = model.params_info
params_info = {key:params_info[key] for key in sorted(params_info)}
keys = np.array([key for key in params_info.keys()])
initial_values = np.array([info['mu'] for info in params_info.values()])
stds = np.array([info['sigma'] for info in params_info.values()])




def setup_directory(dirname):
    if os.path.exists(dirname):
        shutil.rmtree(dirname)
    os.makedirs(dirname)

def log_likelihood(params_value, params_key, model, x, sciantix_folder_path):
    params = {key:value for key, value in zip(params_key, params_value)}
    observation = model._exp(time_point = x)
    y_real = observation[1]
    y_std = observation[2]
    sigma2 = y_std ** 2 
    y_model = model._sciantix(sciantix_folder_path, params, last_value = True)[2]
    # sigma2 = y_std ** 2 +  y_model ** 2 * np.exp(2 * log_f)
    return -0.5 * np.sum((y_real - y_model)**2/sigma2 + np.log(2 * np.pi * sigma2))

def log_prior(params_value, mu, cov):
    diff = params_value - mu
    if len(params_value) == 1:
        return -0.5 * diff ** 2 / cov
    return -0.5 * np.dot(diff, np.linalg.solve(cov, diff))

def log_probability(params_value, params_key, mu, cov, model, x, sciantix_folder_path):
    lp = log_prior(params_value, mu, cov)
    if not np.isfinite(lp):
        return -np.inf
    return lp + log_likelihood(params_value,params_key, model, x, sciantix_folder_path)





def log_probability_kde(params_value, kde, params_key, model, x, sciantix_folder_path):
    lp = np.log(kde(params_value))
    if not np.isfinite(lp):
        return -np.inf
    return lp + log_likelihood(params_value,params_key, model, x, sciantix_folder_path)


setup_directory('Calibration')
calibration_container_path = os.getcwd()
max_params_over_time = [[info['mu'] for info in params_info.values()]]

np.random.seed(42)
time_points = np.linspace(0, max(model.time_exp), 5)
samples = []
for i in range(1,len(time_points)):
    sciantix_folder_path = model._independent_sciantix_folder('Calibration', 0, 0, time_points[i])
    nll = lambda *args: -log_likelihood(*args)
    initial = initial_values
    soln = minimize(nll, initial, args = (keys, model, time_points[i], sciantix_folder_path))

    pos = soln.x + 1e-4 * np.random.randn(10, len(params_info))
    nwalkers, ndim = pos.shape
    mean_values = initial_values
    if ndim > 1:
        covariance = np.zeros((ndim, ndim))
        for i in range(ndim):
            covariance[i,i] = stds[i] ** 2
    else:
        covariance = [stds[0] ** 2]
    
    np.set_printoptions(threshold=np.inf)
    if i == 1:
        sampler = emcee.EnsembleSampler(nwalkers, ndim, log_probability, args = (keys, mean_values, covariance,  model, time_points[i], sciantix_folder_path))
        sampler.run_mcmc(pos, 100, progress = True)
        flat_samples = sampler.get_chain(discard=12, flat=True)
        
    else:
        kde = gaussian_kde(flat_samples)
        sampler = emcee.EnsembleSampler(nwalkers, ndim, log_probability_kde, args = (kde, keys, model, time_points[i], sciantix_folder_path))
        sampler.run_mcmc(pos, 100, process =True)
        flat_samples = sampler.get_chain(discard = 12, flat = True)
    samples.append(flat_samples)
        
for j in range(len(samples)):
    fig = corner.corner(
        samples[j], labels=keys
    )
    plt.show()

# tau = sampler.get_autocorr_time()
# print(tau)

# samples = sampler.get_chain(flat = True)
# plt.hist(samples[:, 0], 100, color="k", histtype="step")
# plt.xlabel(r"$\theta_1$")
# plt.ylabel(r"$p(\theta_1)$")
# plt.gca().set_yticks([])
# plt.show()


# flat_samples = sampler.get_chain(discard=12, flat=True)


# fig = corner.corner(
#     flat_samples, labels=keys
# )
# plt.show()

# p = sampler.get_log_prob(discard= 12, flat = True)
# print(flat_samples.shape, p.shape)
# print(flat_samples)
# print(p)

