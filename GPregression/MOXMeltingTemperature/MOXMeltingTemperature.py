"""

This is a python script to perform the MOX Melting Temperature data fit and correlation update in SCIANTIX.

@author G. Nicodemo

"""

""" ------------------- Import requiered depedencies ------------------- """
import numpy as np
import matplotlib.pyplot as plt
from matplotlib import cm
from matplotlib import rcParams
from mpl_toolkits.mplot3d import Axes3D

import GP_functions
import GP_util
import Correlations

""" ----------------------- Some useful functions -----------------------"""

# Function to extract data
def extract_data(data):
    X_data = data[:, 0]
    SigmaX_data = data[:, 1]
    Pu_data = data[:, 2]
    SigmaPu_data = data[:, 3]
    T_data = data[:, 4]
    SigmaT_data = data[:, 5]
    return X_data, SigmaX_data, Pu_data, SigmaPu_data, T_data, SigmaT_data

# Function to calculate vertical distances between experimental points and the correlation
def vertical_distances(x,y,z, correlation, N):
    distances = []
    for i in range(N):
        dist = z[i] - correlation(x[i],y[i])
        distances = np.append(distances, dist)
    return distances

# Function to perform the whole dataset fit
def whole_dataset_fit(X_data, SigmaX_data, Pu_data, SigmaPu_data, T_data, SigmaT_data, X_plot, Pu_plot, l, sigma, scaling_factor, Np, DoPlot, correlation):
    
    norm_factor = np.max(T_data)
    X_train = np.column_stack((X_data + 2, Pu_data))
    Y_train = T_data / norm_factor
    noise = SigmaT_data / norm_factor

    gx, gy = np.meshgrid(X_plot, Pu_plot)
    X_2D = np.c_[gx.ravel(), gy.ravel()]

    gpr = GP_functions.RBFkernel(l, sigma, scaling_factor, noise)
    gpr.fit(X_train, Y_train)
    log_ml = gpr.log_marginal_likelihood()
    
    mu_s, cov_s = gpr.predict(X_2D, return_cov=True)
    mu_s = mu_s.reshape(gx.shape) * norm_factor
    std_s = np.sqrt(np.diag(cov_s)).reshape(gx.shape) * norm_factor

    if DoPlot:
        plot_3d_surface(gx -2, gy, mu_s, std_s, X_data, Pu_data, T_data, "Whole dataset fit - no data perturbation", "Whole dataset fit without data perturbation", correlation)

    if Np > 0:
        x_dev_perturbed, Pu_perturbed, Tmelt_perturbed  = perturb_data(X_data, Pu_data, T_data, SigmaX_data, SigmaPu_data, SigmaT_data, Np)
        mu_s_perturbed, std_s_perturbed, log_ml_perturbed = compute_perturbed_fit(x_dev_perturbed, Pu_perturbed, Tmelt_perturbed, X_plot, Pu_plot, l, sigma, scaling_factor, SigmaT_data, T_data)
        plot_perturbed_fit(gx -2, gy, mu_s_perturbed, std_s_perturbed, X_data, Pu_data, T_data, Np, DoPlot, "Whole dataset fit after data perturbation")
        
    return mu_s_perturbed, std_s_perturbed, log_ml, log_ml_perturbed

# Function to make a 3D plot of the GP regression 
def plot_3d_surface(gx, gy, mu_s, std_s, X1_data, X2_data, Y_data, title, filename, DoPlot):
    fig, ax = plt.subplots(subplot_kw={"projection": "3d"}, figsize=(8, 8))
    ax.plot_surface(gx, gy, mu_s, cmap=cm.Blues, label='Regression')
    ax.plot_surface(gx, gy, mu_s + 2 * std_s, color='r', alpha=0.2, label='Uncertainty bounds')
    ax.plot_surface(gx, gy, mu_s - 2 * std_s, color='r', alpha=0.2)
    ax.scatter(X1_data, X2_data, Y_data, c='k', marker='o', alpha=.6, label='Data points')
    ax.set_xlabel('Stoichiometry deviation')
    ax.set_ylabel('Pu content')
    ax.set_zlabel('Melting temperature (K)')
    ax.set_title(title)
    ax.legend()
    fig.tight_layout()
    plt.savefig(filename)
    if DoPlot:
        plt.show()
    
# Function to make a comparison plot     
def plot_comparison_fit(gx, gy, mu_s, X1_data, X2_data, Y_data, title, filename, correlation1, correlation2, label1, label2, DoPlot):
    fig, ax = plt.subplots(subplot_kw={"projection": "3d"}, figsize=(8, 8))
    ax.plot_surface(gx, gy, mu_s, cmap=cm.Blues, label='Regression')
    ax.scatter(X1_data, X2_data, Y_data, c='k', marker='o', alpha=.6, label='Data points')
    ax.plot_surface(gx, gy, correlation1(gx, gy), cmap=cm.Reds, alpha=.8, label = label1)
    ax.plot_surface(gx, gy, correlation2(gx, gy), cmap=cm.Greens, alpha=.8, label = label2)
    ax.set_xlabel('Stoichiometry deviation')
    ax.set_ylabel('Pu content')
    ax.set_zlabel('Melting temperature (K)')
    ax.set_title(title)
    ax.legend()
    fig.tight_layout()
    plt.savefig(filename)
    if DoPlot:
        plt.show()

# Routine to perturb data
def perturb_data(X1_data, X2_data, Y_data, SigmaX1_data, SigmaX2_data, SigmaY_data, Np):
    N = X1_data.shape[0]
    
    X1_perturbed = np.array(np.zeros((N, Np)))
    X2_perturbed = np.array(np.zeros((N, Np)))
    Y1_perturbed = np.array(np.zeros((N, Np)))

    for j in range(Np):
        for i in range(N):
            X1_perturbed[i,j] = X1_data[i] + np.random.uniform(-SigmaX1_data[i], SigmaX1_data[i],  1)
            X2_perturbed[i,j] = X2_data[i] + np.random.uniform(-SigmaX2_data[i], SigmaX2_data[i],  1)
            Y1_perturbed[i,j] = Y_data[i]  + np.random.uniform(-SigmaY_data[i],  SigmaY_data[i],   1)

    return X1_perturbed, X2_perturbed, Y1_perturbed

# Function to compute the fit of perturbed data
def compute_perturbed_fit(X1_perturbed, X2_perturbed, Y1_perturbed, X1_plot, X2_plot, l, sigma, scaling_factor, SigmaY_data, Y_data):
    
    Ngrid = len(X1_plot)
    N, Np = np.shape(X1_perturbed)
    
    gx, gy = np.meshgrid(X1_plot, X2_plot)
    X_2D = np.c_[gx.ravel(), gy.ravel()]
    mu_s_perturbed_complete  = np.array(np.zeros((Ngrid ** 2, Np)))
    std_s_perturbed_complete = np.array(np.zeros((Ngrid ** 2, Np)))
    log_ml_perturbed = np.zeros(Np)

    for i in range(Np):
        norm_factor = np.max(Y1_perturbed[:,i])
        X_train = np.column_stack((2+X1_perturbed[:,i], X2_perturbed[:,i]))
        Y_train = Y1_perturbed[:,i]/norm_factor
        noise   = SigmaY_data / norm_factor
        
        gpr = GP_functions.RBFkernel(l, sigma, scaling_factor, noise)
        gpr.fit(X_train, Y_train)
        log_ml_perturbed[i] = gpr.log_marginal_likelihood()

        mu_s_perturbed, cov_s_perturbed = gpr.predict(X_2D, return_cov=True)
        std_s_perturbed = np.sqrt(np.diag(cov_s_perturbed))
        mu_s_perturbed_complete[:, i] = mu_s_perturbed
        std_s_perturbed_complete[:, i] = std_s_perturbed

    mu_s_perturbed_average  = np.average(mu_s_perturbed_complete,  axis=1)
    std_s_perturbed_average = np.average(std_s_perturbed_complete, axis=1)

    mu_s_perturbed_average = mu_s_perturbed_average.reshape(Ngrid, Ngrid) * np.max(Y_data)
    std_s_perturbed_average = std_s_perturbed_average.reshape(Ngrid, Ngrid) * np.max(Y_data)
    
    return mu_s_perturbed_average, std_s_perturbed_average, log_ml_perturbed

# Function to plot perturbed data
def plot_perturbed_fit(gx, gy, mu_s_perturbed, std_s_perturbed, X1_data, X2_data, Y_data, Np, DoPlot, filename):
    fig, ax = plt.subplots(subplot_kw={"projection": "3d"}, figsize=(8, 8))
    ax.plot_surface(gx, gy, mu_s_perturbed, cmap=cm.Blues, label='Regression')
    ax.plot_surface(gx, gy, mu_s_perturbed + 2 * std_s_perturbed , color='r', alpha=0.2, label='Uncertainty bounds')
    ax.plot_surface(gx, gy, mu_s_perturbed - 2 * std_s_perturbed , color='r', alpha=0.2)
    ax.scatter(X1_data, X2_data, Y_data, c='k', marker='o', alpha=.6, label='Data points')
    ax.set_xlabel('Stoichiometry deviation')
    ax.set_ylabel('Pu content')
    ax.set_zlabel('Melting temperature (K)')
    ax.set_title(f'Whole dataset fit - data perturbed {Np} times')
    ax.legend()
    fig.tight_layout()
    plt.savefig(filename)
    if DoPlot:
        plt.show()

# Functions to chose the update options        
def correlation_update_prog1(gx, gy, correlation, mu_s):
    updated_correlation = np.zeros(gx.shape)
    for i in range(gx.shape[0]):
        for j in range(gx.shape[1]):
            if gy[i][j] <= 0.4:
                updated_correlation[i][j] = correlation(gx[i][j], gy[i][j]) + 0.5 * mu_s[i][j]
            elif gy[i][j] > 0.4:
                updated_correlation[i][j] = correlation(gx[i][j], gy[i][j]) + (0.5 + (gy[i][j] - 0.4) * 5 / 6) * mu_s[i][j]
    return updated_correlation

def correlation_update_prog2(gx, gy, correlation, mu_s):
    updated_correlation = np.zeros(gx.shape)
    for i in range(gx.shape[0]):
        for j in range(gx.shape[1]):
            if gy[i][j] <= 0.4:
                updated_correlation[i][j] = correlation(gx[i][j], gy[i][j]) + 0.5 * mu_s[i][j] * (0.1 - gx[i][j] - 0.05) * 20
            elif gy[i][j] > 0.4:
                updated_correlation[i][j] = correlation(gx[i][j], gy[i][j]) + (0.5 + (gy[i][j] - 0.4) * 5 / 6) * mu_s[i][j] * (0.1 - gx[i][j] - 0.05) * 20
    return updated_correlation

""" ------------------- Main function of this script ------------------- """
def GPregression_MOXMeltingTemperature(data, correlation, evaluation_range, l, sigma, scaling_factor, DoPlot = True, update_setting = "half", Np = 10, FixedSeed = "True"):
    """
    data : matrix of N data points x F features
    
    correlation : selected correlation to be updated, see Correlations.py
    
    evaluation_range : range for evaluating the regression, matrix Ngrid x F features
    
    l     = hyperparameter to define the kernel, see GP_util.py
    sigma = hyperparameter to define the kernel, see GP_util.py
    
    scaling_factor = scaling factor to tune the noise
    
    DoPlot = activate/deactivate plots of weight function and updated regression
    
    Update setting = to chose how to mange di update, "half" to stay between the correlation and the data, "False" to perform a fit of data, 
    "prog1" to reduce the weight with the Plutonium content, "prog2" to reduce the weight with the Plutonium content and the stoichiometry deviation
    
    Np = data propagation index, represents the number of times the data is perturbed. The standard value is 10, put 0 to skip data perturbation
    
    """
    
    # Fixed seed option
    if FixedSeed:
        np.random.seed(60)
        
    X_plot, Pu_plot = evaluation_range[:, 0], evaluation_range[:, 1]
    gx, gy = np.meshgrid(X_plot, Pu_plot)
    X_data, SigmaX_data, Pu_data, SigmaPu_data, T_data, SigmaT_data = extract_data(data)
    N = X_data.shape[0]


    if update_setting == "False":
        mu_s, std_s, log_ml, log_ml_perturbed = whole_dataset_fit(X_data, SigmaX_data, Pu_data, SigmaPu_data, T_data, SigmaT_data, X_plot, Pu_plot, l, sigma, scaling_factor, Np, DoPlot, correlation)
        plot_comparison_fit(gx -2, gy, mu_s, X_data, Pu_data, T_data, title = " Results comparison", filename ="Whole dataset fit results comparison", 
                            correlation1 = Correlations.Magni_MOX_Tmelt, correlation2 = Correlations.DiGennaro_MOX_Tmelt, label1 = "Magni Correlation (2021)", label2 = "Di Gennaro Correlation (2024)", DoPlot = DoPlot)
        print("The logarithmic marginal likelihood for unperturbed data is " + str(log_ml))
        print("The logarithmic marginal likelihood for perturbed data are " + str(log_ml_perturbed))
    else:
        distances = vertical_distances(X_data, Pu_data, T_data, correlation, N)
        sc_factor = np.max(np.abs(distances))
        X_train_dist = np.column_stack((X_data + 2, Pu_data))
        Y_train_dist = distances / sc_factor
        noise = np.mean(distances) / sc_factor

        X_2D = np.c_[gx.ravel(), gy.ravel()]

        gpr = GP_functions.RBFkernel(l, sigma, scaling_factor, noise)
        gpr.fit(X_train_dist, Y_train_dist)
        log_ml = gpr.log_marginal_likelihood()
        
        mu_s_dist, cov_s_dist = gpr.predict(X_2D, return_cov=True)
        std_s_dist = np.sqrt(np.diag(cov_s_dist))

        mu_s = mu_s_dist.reshape(gx.shape) * sc_factor
        std_s = std_s_dist.reshape(gx.shape) * sc_factor

        plot_3d_surface(gx -2, gy, mu_s, std_s, X_data, Pu_data, distances, "Distance fitting", "Distance fit without data perturbation", DoPlot)
        print("The logarithmic marginal likelihood for unperturbed distance is " + str(log_ml))

        if Np > 0:
            x_dev_perturbed, Pu_perturbed, Tmelt_perturbed  = perturb_data(X_data, Pu_data, T_data, SigmaX_data, SigmaPu_data, SigmaT_data, Np)
            
            distances_perturbed = np.zeros((N,Np))
            for i in range(Np):
                distances = vertical_distances(x_dev_perturbed[:,i], Pu_perturbed[:,i], Tmelt_perturbed[:,i], correlation, N)
                distances_perturbed[:,i] = distances
            
            mu_s_perturbed, std_s_perturbed, log_ml_perturbed = compute_perturbed_fit(x_dev_perturbed, Pu_perturbed, distances_perturbed, X_plot, Pu_plot, l, sigma, scaling_factor, SigmaT_data, distances)
            plot_perturbed_fit(gx -2, gy, mu_s_perturbed, std_s_perturbed, X_data, Pu_data, distances, Np, DoPlot, "Distance fit after data perturbation")
            print("The logarithmic marginal likelihood for perturbed distances are " + str(log_ml_perturbed))
            
            mu_s = mu_s_perturbed
            std_s = std_s_perturbed

        updated_correlation = np.zeros(gx.shape)
        if update_setting == 'half':
            updated_correlation = correlation(gx - 2, gy) + 0.5 * mu_s
        elif update_setting == 'prog1':
            updated_correlation = correlation_update_prog1(gx -2, gy, correlation, mu_s)
        elif update_setting == 'prog2':
            updated_correlation = correlation_update_prog2(gx -2, gy, correlation, mu_s)
        else:
            updated_correlation = correlation(gx - 2, gy)
            
        mu_s = updated_correlation 

        plot_3d_surface(gx -2, gy, mu_s, std_s, X_data, Pu_data, T_data, "Correlation update", "Updated correlation", DoPlot)

    return mu_s, std_s


