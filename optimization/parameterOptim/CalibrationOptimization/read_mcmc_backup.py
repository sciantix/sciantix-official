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
import corner, ast
import matplotlib.pyplot as plt


scaler_input = joblib.load('scaler_input.pkl')
scaler_output = joblib.load('scaler_output.pkl')

reader = emcee.backends.HDFBackend('backup3.6.h5')

tau = reader.get_autocorr_time()
burnin = int(2 * np.max(tau))
thin = int(0.5 * np.min(tau))
samples = reader.get_chain(discard=burnin, flat=True, thin=thin)
log_prob_samples = reader.get_log_prob(discard=burnin, flat=True, thin=thin)
log_prior_samples = reader.get_blobs(discard=burnin, flat=True, thin=thin)


samples_cluster = []
np.set_printoptions(threshold=np.inf)
samples_cluster.append(samples)

with open('MCMC_samples_fm.txt', 'w') as file:
    for k, array in enumerate(samples_cluster):
        file.write(np.array2string(array, separator=', ') + "\n\n")

# sfs = samples[:,[0,1]]
# sfs = scaler_input.inverse_transform(sfs)
samples[:,1] = np.exp(samples[:,1])
corner.corner(samples, labels = ['sf_actEnergy', 'sf_preExp', 'fm'])
plt.show()






