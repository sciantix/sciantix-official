import sys
import os
import numpy as np
import matplotlib.pyplot as plt

# Path to the compiled sciantixModule 
module_path = os.path.abspath(os.path.join(os.path.dirname(__file__), '..','..', 'build', 'python'))
if module_path not in sys.path:
    # Add the module to the system path
    sys.path.append(module_path)

# Importation of the Module
import sciantixModule


def main(k,exact_solution):
    """This fonction do the MMS verification of the Decay Solver ( dphi/dt = -k * phi**2 )"""

    # Parameters    
    t0 = 1
    t_end = 2
    phi0 = exact_solution(t0)

    # all the increments 
    increments = [0.1, 0.05, 0.025, 0.0125, 0.00625, 0.003125]

    # for the plot 
    solutions = []
    errors = []

    # Creating an instance of the Solver
    solver = sciantixModule.Solver()
    
    for h in increments:
        t = t0
        phi = phi0
        while t < t_end:
            phi = solver.BinaryInteraction(phi,  k(t), h )
            t += h
        
        # Verification
        numerical_solution = phi
        
        exact = exact_solution(t_end)
        error = abs(numerical_solution - exact)
        
        # added to a list for plot 
        solutions.append(numerical_solution)
        errors.append(error)
            
        print(f"Increment: {h}, Numerical: {numerical_solution}, Exact: {exact}, Error: {error}")

    # Calculate Order of Convergence (OC)
    errors = np.array(errors)
    increments = np.array(increments)

    # the slope coefficient the log of the increments/errors
    OC = np.polyfit(np.log(increments), np.log(errors), 1)[0] 

    print(f"Order of Convergence: {OC}")

    plt.figure(figsize=(10, 6))
    plt.loglog(increments, errors, 'bo-')
    plt.xlabel('size of steps of time (h)')
    plt.ylabel('Error')
    plt.title('Log(error) vs Log(h)')
    plt.grid(True)

    # plot the solution for the minimum step 
    h_min = min(increments)
    t_values = np.arange(t0, t_end + h_min, h_min)
    phi_values = [phi0]
    for t in t_values[1:]:
        phi_values.append(solver.BinaryInteraction(phi_values[-1], k(t), h_min))

    t_exact = np.linspace(t0, t_end, 100)
    phi_exact = exact_solution(t_exact)

    plt.figure(figsize=(10, 6))
    plt.plot(t_values, phi_values, 'b-', label='Numerical')
    plt.plot(t_exact, phi_exact, 'r--', label='Exact')
    plt.xlabel('t')
    plt.ylabel('φ(t)')
    plt.title('Numerical vs Exact Solution')
    plt.legend()
    plt.grid(True)

    # for test purposes and verification of results 
    def calculate_local_oc(h1, h2, e1, e2):
        return np.log(e1 / e2) / np.log(h1 / h2)

    local_ocs = []
    for i in range(len(increments) - 1):
        oc = calculate_local_oc(increments[i], increments[i+1], errors[i], errors[i+1])
        local_ocs.append(oc)
        print(f"OC between {increments[i]} and {increments[i+1]}: {oc}")

    # evolution of OC between steps
    plt.figure(figsize=(10, 6))
    plt.plot(increments[:-1], local_ocs, 'ro-')
    plt.xlabel('size of steps of time (h)')
    plt.ylabel('Order of Convergence Local')
    plt.title('evolution of the Order of Convergence')
    plt.grid(True)
    plt.show()


if __name__ == "__main__":
    print("This file should be run from mainVerification.py")