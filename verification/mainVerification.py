import verificationIntegrator
import verificationDecay
import verificationLimitedGrowth
import verificationBinaryInteraction
import SpectralDiffusionVerification
import numpy as np
from scipy.special import lambertw

# Define the functions and their exact solutions for the integration solver y' = + S.
functions = { 
    0: {"name": "F0: 3 * t²",
        "F": lambda t: 3 * (t ** 2),
        "exact": lambda t: t ** 3},
    1: {
        "name": "F1: t * sin(t)",
        "F": lambda t: t * np.sin(t),
        "exact": lambda t: np.sin(t) - t * np.cos(t),
    },
    2: {
        "name": "F2: t * e^t * sin(t)",
        "F": lambda t: t * np.exp(t) * np.sin(t),
        "exact": lambda t: 0.5 * np.exp(t)* (t * np.sin(t) - t * np.cos(t) + np.cos(t))- 0.5, # -0.5 
    },
}

# Decay solver parameters y' = - L y + S
decay_parameters = {
    0: {"L": lambda t: -2 / t,
        "S": lambda t: 0},

    1: {"L": lambda t: -1 / t,
        "S": lambda t: t * np.cos(t)},

    2: {"L": lambda t: -(t + 1) / t,
        "S": lambda t: np.exp(t) * t * np.cos(t)},
}

# Limited growth solver parameters y' = k / y + S
growth_parameters = {
    0: {"k": lambda t: 18 * t ** 3,
        "S": lambda t: 0},

    1: {"k": lambda t: t * np.sin(t) ** 2, 
        "S": lambda t: t * np.cos(t)},

    2: {
        "k": lambda t: t ** 2 * np.exp(2 * t) * np.sin(t) ** 2,
        "S": lambda t: np.exp(t) * t * np.cos(t),
    },
}

# Binary interaction solver parameters y' = -k y**2 
binary_parameters = {
    0: {"k": lambda t: -2 / (3 * t ** 3)},

    1: {"k": lambda t: -(np.sin(t) + t * np.cos(t)) / (t * np.sin(t)) ** 2},

    2: {"k": lambda t: -((t + 1) * np.sin(t) + t * np.cos(t))/ (np.exp(t) * (t * np.sin(t)) ** 2)},
}

spectral_parameters = {
    0:{
        "N_modes" : 5.0 ,
       "D": lambda t : 1/6(1-np.exp(-t)) ,
       "r" : 1.0,
       "production" : 1, 
       "loss_rate" : lambda t: (-np.exp(-t))/(1-np.exp(-t)),
    },
    # 1: {
    #     "N_modes": 5.0,
    #     "D": D,
    #     "r": 1.5,
    #     "production": S, # S 
    #     "loss_rate":  -np.pi**2*D - D + np.pi*D/(r*np.tan(np.pi*r)) + S*np.exp(-D*t)*np.tan(np.pi*t)**2/(2*np.sin(np.pi*r)) + S*np.exp(-D*t)/(2*sin(np.pi*r)) + 2*np.pi*np.sin(np.pi*t)/np.cos(np.pi*t)**3 - 2*np.pi*np.tan(np.pi*t)**3, # L
    # },
}
def spectral_parameters_0(r, t, S, D, Nmodes):
    parameters = [5 , 0.1 , 1.0, 0.0, 0.1 * (np.pi **2 - 1) ]
    return parameters
def spectral_parameters_1(r, t, S, D, Nmodes):
    epsilon = 1e-10  # Une petite valeur pour éviter la division par zéro
    
    # Éviter la division par zéro dans tan(pi*r)
    tan_term = np.tan(np.pi*r) if abs(np.sin(np.pi*r)) > epsilon else np.sign(np.sin(np.pi*r)) * 1e10
    
    # Éviter la division par zéro dans sin(pi*r)
    sin_term = np.sin(np.pi*r) if abs(np.sin(np.pi*r)) > epsilon else epsilon
    
    # Éviter la division par zéro dans cos(pi*t)
    cos_term = np.cos(np.pi*t) if abs(np.cos(np.pi*t)) > epsilon else epsilon
    
    L = (-np.pi**2*D - D + np.pi*D/(r*tan_term) 
         + S*np.exp(-D*t)*np.tan(np.pi*t)**2/(2*sin_term) 
         + S*np.exp(-D*t)/(2*sin_term) 
         + 2*np.pi*np.sin(np.pi*t)/(cos_term**3) 
         - 2*np.pi*np.tan(np.pi*t)**3)
    
    parameters = [Nmodes, D, r, S, L]
    return parameters

def manufactured_solution_spectral_0(r, t, D):
    return (np.exp(-D * t) * np.sin(np.pi * r))

def manufactured_solution_spectral_1(r, t, D):
    return np.exp(-D * t) * np.sin(np.pi * r) * (1 + np.cos(2 * np.pi * t))

# Verification functions
def integrator_verification():
    function_id = get_function_choice("Integrator")
    chosen_function = functions[function_id]
    verificationIntegrator.main(chosen_function["F"], chosen_function["exact"])


def decay_verification():
    function_id = get_function_choice("Decay")
    chosen_function = functions[function_id]
    chosen_decay = decay_parameters[function_id]
    verificationDecay.main(chosen_decay["L"], chosen_decay["S"], chosen_function["F"])


def limited_growth_verification():
    function_id = get_function_choice("Limited Growth")
    chosen_function = functions[function_id]
    chosen_growth = growth_parameters[function_id]
    verificationLimitedGrowth.main( chosen_growth["k"], chosen_growth["S"], chosen_function["F"])


def binary_verification():
    function_id = get_function_choice("Binary Interaction")
    chosen_function = functions[function_id]
    chosen_binary = binary_parameters[function_id]

    print("---------- Choose the mode of Binary Interaction solver: ----------")
    print("[0] Old model")
    print("[1] New model")

    mode_id = get_choice("Enter the chosen mode (0, 1) = ", 0, 1)
    verificationBinaryInteraction.main(
        chosen_binary["k"], chosen_function["F"], mode_id
    )

def Spectral_Diffusion_verification():
    print("---------- Choose the spectral function: ----------")
    print("[0] Simple spectral function")
    print("[1] Complex spectral function")

    function_id = get_choice("Enter the chosen function (0, 1) = ", 0, 1)

    if function_id == 0:
        chosen_spectral = spectral_parameters_0
        solution = manufactured_solution_spectral_0
    elif function_id == 1:
        chosen_spectral = spectral_parameters_1
        solution = manufactured_solution_spectral_1

    SpectralDiffusionVerification.main(solution, chosen_spectral)


# Helper function to get the user's function choice
def get_function_choice(solver_name):
    print("") # for style and more visibility
    print(f"---------- Choose the function for MMS verification of {solver_name} solver: ----------" )
    print("") # for style and more visibility
    for i, details in functions.items():
        print(f"[{i}] {details['name']}")

    return get_choice("Enter the chosen function (0, 1, 2) = ", 0, 2)


# Helper function to get a valid user choice
def get_choice(prompt, min_value, max_value):
    while True:
        try:
            choice = int(input(prompt))
            if min_value <= choice <= max_value:
                return choice
            else:
                print(f"Invalid choice. Please enter a number between {min_value} and {max_value}.")
        except ValueError:
            print("Invalid input. Please enter a valid number.")


if __name__ == "__main__":
    print("---------- Choose a solver: ----------")
    print("[0] Integrator")
    print("[1] Decay")
    print("[2] Binary Interaction")
    print("[3] Limited Growth")
    print("[4] Spectral Diffusion")
    print("[5] All")

    solver_id = get_choice("Enter the chosen solver (0, 1, 2, 3, 4, 5) = ", 0, 5)

    if solver_id == 0:
        integrator_verification()
    elif solver_id == 1:
        decay_verification()
    elif solver_id == 2:
        binary_verification()
    elif solver_id == 3:
        limited_growth_verification()
    elif solver_id == 4:
        Spectral_Diffusion_verification()
    elif solver_id == 5:
        integrator_verification()
        decay_verification()
        binary_verification()
        limited_growth_verification()
        Spectral_Diffusion_verification()