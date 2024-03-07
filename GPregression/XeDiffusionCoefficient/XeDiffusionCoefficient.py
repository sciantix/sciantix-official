"""

This is a python script to execute the perform the Xe diffusion coefficient correlation update in sciantix.

@author G. Nicodemo

"""

""" ------------------- Import requiered depedencies ------------------- """
import numpy as np
import matplotlib.pyplot as plt
from matplotlib import cm
from matplotlib import rcParams
from mpl_toolkits.mplot3d import Axes3D

import GP_functions

""" ------------------- Main function of this script ------------------- """
def GPregression_XeDiffusionCoefficient(data, correlation, evaluation_range, l, sigma, scaling_factor, DoPlot = True, update_setting = "half"):
    """
    data : matrix of N data points x F features
    
    correlation : selected correlation to be updated, see Correlations.py
    
    evaluation_range : range for evaluating the regression, matrix Ngrid x F features
    
    l     = hyperparameter to define the kernel, see GP_util.py
    sigma = hyperparameter to define the kernel, see GP_util.py
    
    scaling_factor = scaling factor to tune the noise
    
    DoPlot = activate/deactivate plots of weight function and updated regression
    
    Update setting = to chose how to mange di update, "half" to stay between the correlation and the data, "prog" to stay closer to data if they are far from the correlation
    
    """
    # Number of data
    N = data.shape[0]
    
    Tdata = data[:,0] # 1e4/T (K)
    Ddata = data[:,1] # m2/s
    Fdata = data[:,2] # fiss/m3s
    
    def vertical_distances(x,y,z,):
        distances = []
        for i in range(N):
            dist = z[i] - np.log10(correlation(x[i],y[i]))
            distances.append(dist)
        return np.array(distances).reshape(-1,1)
    
    distances = vertical_distances(1e4/Tdata, Fdata, np.log10(Ddata))
    
    # Training data
    X_2D_dist = np.column_stack((Tdata, np.log10(Fdata)))
    Y_2D_dist = distances
    
    noise = 0.001

    T_plot = evaluation_range[:,0]
    F_plot = evaluation_range[:,1]

    gx, gy = np.meshgrid(T_plot, F_plot)
    X_2D = np.c_[gx.ravel(), gy.ravel()]

    # Kernel setup
    gpr = GP_functions.RBFkernel(l, sigma, scaling_factor, noise)
    gpr.fit(X_2D_dist, Y_2D_dist)

    # Compute posterior mean and covariance
    mu_s, cov_s = gpr.predict(X_2D, return_cov=True)
    std_s = np.sqrt(np.diag(cov_s))
    
    # Da finire
    Trust_function = 0
    
    if update_setting == "half":
        updated_correlation = np.log10(correlation(1e4/gx, 10 ** gy)) + 0.5*mu_s.reshape(gx.shape)
    elif update_setting == "prog":
        updated_correlation = np.log10(correlation(1e4/gx, 10 ** gy)) - Trust_function* mu_s.reshape(gx.shape)
    else:
        updated_correlation = np.log10(correlation(1e4/gx, 10 ** gy))
    
    # Plot the weight function 
    fig, ax = plt.subplots(subplot_kw={"projection": "3d"},figsize=(10, 10))

    ax.plot_surface(gx, gy, mu_s.reshape(gx.shape), cmap=cm.Blues, label = 'Data Fit', alpha=.4)
    ax.plot_surface(gx, gy, (mu_s + 2 * std_s).reshape(gx.shape), color='r', alpha=0.2, label = 'Uncertainty buonds')
    ax.plot_surface(gx, gy, (mu_s - 2 * std_s).reshape(gx.shape), color='r', alpha=0.2)

    ax.scatter(Tdata, np.log10(Fdata), distances, c='k', marker='o', alpha=.6, label = 'Data points')

    ax.set_xlabel('$10^4/T$')
    ax.set_ylabel('$\log_{10}(\dot{F})$')
    ax.set_zlabel('$\log_{10}(D)$')
    ax.set_title('Weight function')
    ax.legend()
    fig.tight_layout()
    plt.savefig('Weight function')
    if DoPlot:
        plt.show()
    
    # Plot the 
    fig, ax3 = plt.subplots(figsize=(10, 10))

    x = np.linspace(min(gx.flatten()), max(gx.flatten()), 100)
    y = np.linspace(min(gy.flatten()), max(gy.flatten()), 100)
    x, y = np.meshgrid(x, y)

    # Evaluate the surface function at these points to obtain z values
    z = mu_s.reshape(gx.shape)

    # Create contour plot
    contour = ax3.contour(x, y, z, cmap='viridis')
    ax3.set_title('Contour of the weight function')
    ax3.set_xlabel('$10^4/T$')
    ax3.set_ylabel('$\log_{10}(\dot{F})$')
    plt.colorbar(contour, ax=ax3, label='$\log_{10}(D)$')

    ax3.set_title('Contour of the weight function')
    ax3.set_xlabel('$10^4/T$')
    ax3.set_ylabel('$\log_{10}(\dot{F})$')
    cbar = plt.colorbar(contour, ax=ax)
    cbar.set_label('$\log_{10}(D)$')
    plt.savefig('Contour of the weight function')
    if DoPlot:
        plt.show()

    # Plot the updated correlation
    fig, ax = plt.subplots(subplot_kw={"projection": "3d"},figsize=(10, 10))

    ax.plot_surface(gx, gy, updated_correlation , cmap=cm.Blues, label = 'Data Update', alpha=.4)
    ax.plot_surface(gx, gy, updated_correlation + 2 * std_s.reshape(gx.shape), color='r', alpha=0.2, label = 'Uncertainty buonds')
    ax.plot_surface(gx, gy, updated_correlation + 2 * std_s.reshape(gx.shape), color='r', alpha=0.2)

    ax.scatter(Tdata, np.log10(Fdata), np.log10(Ddata), c='k', marker='o', alpha=.6, label = 'Data points')
    ax.plot_surface(gx, gy, np.log10(correlation(1e4/gx, 10 ** gy)), cmap=cm.Greens, label = 'Turnbull diffusion coefficient', alpha=0.5)

    ax.set_xlabel('$10^4/T$')
    ax.set_ylabel('$\log_{10}(\dot{F})$')
    ax.set_zlabel('$\log_{10}(D)$')
    ax.set_title('2D Fit of Fission Gas diffusion coefficient')
    ax.legend()
    fig.tight_layout()
    plt.savefig('2D Diffusion Coefficient Correlation Update')
    if DoPlot:
        plt.show()

    return updated_correlation, std_s.reshape(gx.shape)
    

