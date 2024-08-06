import sys
import os
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

module_path = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'build', 'python'))
if module_path not in sys.path:
    sys.path.append(module_path)

import sciantixModule


def find_scaling_factor(solution, solver, r_values, t_end, D, S, L, parameters, h):
    # Calculate the exact solution at t_end
    exact_solutions = solution(r_values, t_end, D)
    
    # Calculate the numerical solution
    numerical_solutions = []
    t = 0
    while t < t_end:
        current_solutions = []
        for r in r_values:
            initial_condition = solution(r, t, D)
            numerical_solution = solver.SpectralDiffusion(initial_condition, parameters, float(h))
            current_solutions.append(numerical_solution)
        numerical_solutions.append(current_solutions)
        t += h
    
    final_numerical_solution = numerical_solutions[-1]
    
    # Find the scaling factor
    scaling_factor = np.mean(exact_solutions / final_numerical_solution)
    return scaling_factor


def main(solution, param):
    t0 = 0.0
    t_end = 1.0
    increments = [0.1, 0.05, 0.025, 0.0125, 0.00625, 0.003125]
    
    # Définir les valeurs initiales pour r, S, D, et Nmodes
    r = 1.5
    S = 1.0  # Vous pouvez ajuster cette valeur selon vos besoins
    D = 0.1  # Vous pouvez ajuster cette valeur selon vos besoins
    Nmodes = 5.0
    
    # Utiliser la fonction param pour obtenir les paramètres
    parameters = param(r, t0, S, D, Nmodes)
    
    r_values = np.linspace(0, parameters[2], 100)  # parameters[2] est r
    solver = sciantixModule.Solver()

    # Le reste du code reste inchangé
    scaling_factor = find_scaling_factor(solution, solver, r_values, t_end, D, S, parameters[4], parameters, increments[-1])
    print(f"Calculated scaling factor: {scaling_factor}")

    errors = []
    final_numerical_solution = None
    
    for h in increments:
        t = t0
        numerical_solutions = []

        while t < t_end:
            current_solutions = []
            for r in r_values:
                # Calculer les paramètres actuels
                current_parameters = param(r, t, S, D, Nmodes)
                current_D = current_parameters[1]  # D est à l'index 1
                current_S = current_parameters[3]  # S est à l'index 3
                current_L = current_parameters[4]  # L est à l'index 4
                
                initial_condition = solution(r, t, current_D)
                try:
                    numerical_solution = solver.SpectralDiffusion(initial_condition, current_parameters, float(h))
                    scaled_solution = numerical_solution * scaling_factor  
                    current_solutions.append(scaled_solution)
                except Exception as e:
                    print(f"Error at r = {r}, t = {t}: {e}")
                    print(f"Parameters: {initial_condition}, {current_parameters}, {h}")
                    return
                
            numerical_solutions.append(current_solutions)
            t += h
        
        # Le reste du code reste inchangé
        
        final_numerical_solution = numerical_solutions[-1]
        
        exact_solutions = solution(r_values, t_end, parameters[1])
        
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
    Z_exact = np.array([solution(r_values, t, D) for t in np.linspace(t0, t_end, len(numerical_solutions))]).T
    Z_numerical = np.array(numerical_solutions).T

    surf_exact = ax.plot_surface(R, T, Z_exact, cmap='coolwarm', alpha=0.7)
    surf_numerical = ax.plot_surface(R, T, Z_numerical, cmap='viridis', alpha=0.7)

    ax.set_xlabel('Radial Position (r)')
    ax.set_ylabel('Time (t)')
    ax.set_zlabel('Solution')
    ax.set_title('Manufactured and Numerical Solutions')

    fig.colorbar(surf_exact, shrink=0.5, aspect=5, label='Exact Solution')
    fig.colorbar(surf_numerical, shrink=0.5, aspect=5, label='Numerical Solution')

    ax.legend([surf_exact, surf_numerical], ['Exact', 'Numerical'])

    plt.figure(figsize=(10, 6))
    plt.plot(r_values, solution(r_values, t_end, D), 'b-', label='Exact Solution')
    plt.plot(r_values, final_numerical_solution, 'r--', label='Numerical Solution')
    plt.xlabel('Radial Position (r)')
    plt.ylabel('Solution')
    plt.title(f'Comparison of Exact and Numerical Solutions at t = {t_end}')
    plt.legend()
    plt.grid(True)
    
    def calculate_local_oc(h1, h2, e1, e2):
        return np.log(e1 / e2) / np.log(h1 / h2)

    # Calculate and plot local order of convergence
    local_ocs = []
    for i in range(len(increments) - 1):
        oc = calculate_local_oc(increments[i], increments[i+1], errors[i], errors[i+1])
        local_ocs.append(oc)
        print(f"OC between {increments[i]} and {increments[i+1]}: {oc}")

    # Evolution of OC between steps
    plt.figure(figsize=(10, 6))
    plt.plot(increments[:-1], local_ocs, 'ro-')
    plt.xlabel('Size of time steps (h)')
    plt.ylabel('Local Order of Convergence')
    plt.title('Evolution of the Order of Convergence')
    plt.grid(True)
    plt.show()

if __name__ == "__main__":
    main()
