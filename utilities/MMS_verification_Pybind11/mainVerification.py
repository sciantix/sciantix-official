import numpy as np

# import all the files for verification
import verificationIntegrator
import verificationDecay
import verificationLimitedGrowth
import verificationBinaryInteraction
import SpectralDiffusionVerification


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

# Verification functions
def integrator_verification():
    """ Used for the integrator verifcation"""
    # get the function 
    function_id = get_function_choice("Integrator")
    chosen_function = functions[function_id] # get the parameters


    verificationIntegrator.main(chosen_function["F"], chosen_function["exact"])


def decay_verification():
    """ Used for the Decay verifcation"""
    function_id = get_function_choice("Decay")
    # get the function
    chosen_function = functions[function_id]
    chosen_decay = decay_parameters[function_id] # get the parameters

    verificationDecay.main(chosen_decay["L"], chosen_decay["S"], chosen_function["F"])


def limited_growth_verification():
    """ Used for the Limited growth verification """
    function_id = get_function_choice("Limited Growth")
    # get the function
    chosen_function = functions[function_id]
    chosen_growth = growth_parameters[function_id] # get the parameters

    verificationLimitedGrowth.main( chosen_growth["k"], chosen_growth["S"], chosen_function["F"])


def binary_verification():
    """ Used for the binary verification """
    # get the function
    function_id = get_function_choice("Binary Interaction")
    chosen_function = functions[function_id]
    chosen_binary = binary_parameters[function_id] # get the parameters
    
    verificationBinaryInteraction.main(chosen_binary["k"], chosen_function["F"])


# Helper function to get the user's function choice
def get_function_choice(solver_name):
    """ This function is used to select a function and return the inputed choice"""
    print("") # for style and more visibility
    print(f"---------- Choose the function for MMS verification of {solver_name} solver: ----------" )
    print("") # for style and more visibility
    for i, details in functions.items():
        print(f"[{i}] {details['name']}")

    return get_choice("Enter the chosen function (0, 1, 2) = ", 0, 2)


# Helper function to get a valid user choice
def get_choice(prompt, min_value, max_value):
    """ This function is used to get the input value between a Min and a Max value"""
    while True:
        try:
            choice = int(input(prompt))
            if min_value <= choice <= max_value:
                return choice
            else:
                print(f"Invalid choice. Please enter a number between {min_value} and {max_value}.")
        except ValueError:
            print("Invalid input. Please enter a valid number.")

# Main
if __name__ == "__main__":
    # Print out the Menu for each solver 
    print("---------- Choose a solver: ----------")
    print("[0] Integrator")
    print("[1] Decay")
    print("[2] Binary Interaction")
    print("[3] Limited Growth")
    print("[4] Spectral Diffusion")
    print("[5] All")

    # get the imputed value
    solver_id = get_choice("Enter the chosen solver (0, 1, 2, 3, 4, 5) = ", 0, 5)

    # switch for each solver 
    if solver_id == 0:
        integrator_verification()
    elif solver_id == 1:
        decay_verification()
    elif solver_id == 2:
        binary_verification()
    elif solver_id == 3:
        limited_growth_verification()
    elif solver_id == 4:
        SpectralDiffusionVerification.Spectral_Diffusion_verification()
    elif solver_id == 5: # every solver at once
        integrator_verification()
        decay_verification()
        binary_verification()
        limited_growth_verification()
        SpectralDiffusionVerification.Spectral_Diffusion_verification()