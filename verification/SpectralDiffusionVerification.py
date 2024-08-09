import sys
import os
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

# Ajouter le chemin du module compilé
module_path = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'build', 'python'))
if module_path not in sys.path:
    sys.path.append(module_path)

import sciantixModule

# Paramètres spectraux
spectral_parameters = {
    0: {
        "N_modes": 5.0,
        "D": lambda t: 1/6*(1-np.exp(-t)),
        "r_max": 1.0,  # Valeur maximale de r
        "production": 1, 
        "loss_rate": lambda t: (-np.exp(-t))/(1-np.exp(-t)) if t != 0 else -1e6,  # éviter -inf
    },
}

# Solution manufacturée
def manufactured_solution_spectral_0(r, t, D):
    return np.exp(-t) * (1 - r**2)

# Fonction pour faire varier r en fonction du temps
def r(t, t_final):
    return spectral_parameters[0]["r_max"] * (t / t_final)

# Paramètres pour le solver
def param(t):
    p = spectral_parameters[0]
    D_value = p["D"](t)
    
    loss_rate_value = p["loss_rate"](t)
    # Eviter les valeurs non définies
    if np.isinf(loss_rate_value) or np.isnan(loss_rate_value):
        loss_rate_value = -1e6
    return [
        p["N_modes"],
        D_value,
        r(t, t_final),  # Utilisation de la fonction r(t, t_final)
        p["production"],
        loss_rate_value
    ]

# Initialisation des conditions
initial_conditions = [0.0] * int(spectral_parameters[0]["N_modes"])

# Paramètres temporels
t_final = 1.0
dt = 0.1
increments = [0.1, 0.05, 0.025, 0.0125, 0.00625, 0.003125]

# Initialiser le solver
solver = sciantixModule.Solver()

def main(solution, param):
    t0 = 0.0
    t_end = 1.0
    
    # Définir les valeurs initiales pour r, S, D, et Nmodes
    r_values = np.linspace(0, spectral_parameters[0]["r_max"], 100)
    
    errors = []
    final_numerical_solution = None

    for h in increments:
        t = t0
        numerical_solutions = []

        while t <= t_end:  # Utiliser <= pour inclure t_end
            current_solutions = []
            for r_value in r_values:
                current_parameters = param(t)
                current_D = current_parameters[1] 
                
                
                initial_condition = solution(r_value, t, current_D)
                try:
                    numerical_solution = solver.SpectralDiffusion(initial_condition, current_parameters, float(h))
                    current_solutions.append(numerical_solution)
                except Exception as e:
                    print(f"Error at r = {r_value}, t = {t}: {e}")
                    print(f"Parameters: {initial_condition}, {current_parameters}, {h}")
                    return
                
            numerical_solutions.append(current_solutions)
            t += h

        final_numerical_solution = numerical_solutions[-1]

        exact_solutions = solution(r_values, t_end, spectral_parameters[0]["D"](t_end))

        errors_per_r = np.abs(np.array(final_numerical_solution) - exact_solutions)
        max_error = np.max(errors_per_r)
        errors.append(max_error)
        
        print(f"Increment: {h}, Max Error: {max_error}")

    order_of_convergence = np.polyfit(np.log(increments), np.log(errors), 1)[0]
    print(f"Order of Convergence: {order_of_convergence}")

    plt.figure(figsize=(10, 6))
    plt.loglog(increments, errors, 'bo-')
    plt.xlabel('Time Step (h)')
    plt.ylabel('Maximum Error')
    plt.title('Error vs Time Step')
    plt.grid(True)

    fig = plt.figure(figsize=(12, 8))
    ax = fig.add_subplot(111, projection='3d')

    T, R = np.meshgrid(np.linspace(t0, t_end, len(numerical_solutions)), r_values)
    Z_exact = np.array([solution(r_values, t, spectral_parameters[0]["D"](t)) for t in np.linspace(t0, t_end, len(numerical_solutions))]).T
    Z_numerical = np.array(numerical_solutions).T

    surf_exact = ax.plot_surface(R, T, Z_exact, cmap='coolwarm', alpha=0.7)
    surf_numerical = ax.plot_surface(R, T, Z_numerical, cmap='viridis', alpha=0.7)

    ax.set_xlabel('Radial Position (r)')
    ax.set_ylabel('Time (t)')
    ax.set_zlabel('Solution')
    ax.set_title('Manufactured and Numerical Solutions')

    fig.colorbar(surf_exact, shrink=0.5, aspect=5, label='Exact Solution')
    fig.colorbar(surf_numerical, shrink=0.5, aspect=5, label='Numerical Solution')

    plt.figure(figsize=(10, 6))
    plt.plot(r_values, solution(r_values, t_end, spectral_parameters[0]["D"](t_end)), 'b-', label='Exact Solution')
    plt.plot(r_values, final_numerical_solution, 'r--', label='Numerical Solution')
    plt.xlabel('Radial Position (r)')
    plt.ylabel('Solution')
    plt.title(f'Comparison of Exact and Numerical Solutions at t = {t_end}')
    plt.legend()
    plt.grid(True)
    
    def calculate_local_oc(h1, h2, e1, e2):
        return np.log(e1 / e2) / np.log(h1 / h2)

    local_ocs = []
    for i in range(len(increments) - 1):
        oc = calculate_local_oc(increments[i], increments[i+1], errors[i], errors[i+1])
        local_ocs.append(oc)
        print(f"OC between {increments[i]} and {increments[i+1]}: {oc}")

    plt.figure(figsize=(10, 6))
    plt.plot(increments[:-1], local_ocs, 'ro-')
    plt.xlabel('Size of time steps (h)')
    plt.ylabel('Local Order of Convergence')
    plt.title('Evolution of the Order of Convergence')
    plt.grid(True)
    plt.show()

if __name__ == "__main__":
    main(manufactured_solution_spectral_0, param)
