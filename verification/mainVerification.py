import verificationIntegrator
import verificationDecay
import verificationLimitedGrowth
import verificationBinaryInteraction
import numpy as np

# initialisation of the functions
def F1(t):
    return 3 * (t ** 2)

def F2(t):
    return t * np.sin(t)

def F3(t):
    return t * np.exp(t)* np.sin(t)

# solution for the Integrator 
def exact_solution1(t):
    return t**3

def exact_solution2(t):
    return np.sin(t) - t * np.cos(t)  

def exact_solution3(t):
    return 1/2 *np.exp(t) *(t* np.sin(t) - t* np.cos(t) + np.cos(t)) - 0.5 # 0.5 is the constant of integration  to match the graph

def IntegratorVerification (): 

    print("---------- Choose the function for MMS verification : ----------")
    print("[0] F0 : 3 * t²")
    print("[1] F1 : t * sin(t)")
    print("[2] F2 : t * e^t * sin(t)")
    functionId = input("Enter the chosen function (0, 1, 2) = ")
    functionId = int(functionId)
    if functionId == 0:
        print("You chose option 0")
        chosen_F = F1
        exact_solution = exact_solution1
    elif functionId == 1:
        print("You chose option 1")
        chosen_F = F2
        exact_solution = exact_solution2
    elif functionId == 2:
        print("You chose option 2")
        chosen_F = F3
        exact_solution = exact_solution3
    else:
        print(f"{functionId} is not a valid choice")
        exit()
    # execution fo the verification
    verificationIntegrator.main(chosen_F, exact_solution)

# Lambda for the F1 function
def L1(t):
    return  - 2 / t 
def S1(t):
    return 0

# Lambda for the F2 function
def L2(t):
    return - 1/t
def S2(t):
    return t * np.cos(t)

# Lambda for the F3 function
def L3(t):
    return - (t+1)/t 
def S3(t):
    return np.exp(t) * t * np.cos(t)

# function for the Decay MMS verification 
def DecayVerification():
    print("---------- Choose the function for MMS verification of Decay solver: ----------")
    print("[0] F0 : 3 * t²")
    print("[1] F1 : t * sin(t)")
    print("[2] F2 : t * e^t * sin(t)")
    functionId = input("Enter the chosen function (0, 1, 2) = ")
    functionId = int(functionId)
    if functionId == 0:
        print("You chose option 0")
        chosen_L = L1
        chosen_S = S1
        exact_solution = F1
    elif functionId == 1:
        print("You chose option 1")
        chosen_L = L2
        chosen_S = S2
        exact_solution = F2
    elif functionId == 2:
        print("You chose option 2")
        chosen_L = L3
        chosen_S = S3
        exact_solution = F3
    else:
        print(f"{functionId} is not a valid choice")
        exit()
    # execution fo the verification
    verificationDecay.main(chosen_L, chosen_S, exact_solution)

# data for the LimitedGrowth solver for F1
def kGrowth1(t):
    return 18 * t **3
def SGrowth1(t):
    return 0 
# data for the LimitedGrowth solver for F2
def kGrowth2(t):
    return t * np.sin(t)**2
def SGrowth2(t):
    return t * np.cos(t)
# data for the LimitedGrowth solver for F3
def kGrowth3(t):
    return t**2 * np.exp(2 * t) * np.sin(t)**2
def SGrowth3(t):
    return np.exp(t) * t * np.cos(t)


# function for the Limited Growth MMS verification 
def LimitedGrowthVerification():
    print("---------- Choose the function for MMS verification of Limited Growth solver: ----------")
    print("[0] F0 : 3 * t²")
    print("[1] F1 : t * sin(t)")
    print("[2] F2 : t * e^t * sin(t)")
    functionId = input("Enter the chosen function (0, 1, 2) = ")
    functionId = int(functionId)
    if functionId == 0:
        print("You chose option 0")
        chosen_k = kGrowth1
        chosen_S = SGrowth1
        exact_solution = F1
    elif functionId == 1:
        print("You chose option 1")
        chosen_k = kGrowth2
        chosen_S = SGrowth2
        exact_solution = F2
    elif functionId == 2:
        print("You chose option 2")
        chosen_k = kGrowth3
        chosen_S = SGrowth3
        exact_solution = F3
    else:
        print(f"{functionId} is not a valid choice")
        exit()
    # execution fo the verification
    verificationLimitedGrowth.main(chosen_k, chosen_S , exact_solution)

# Values for the Binary verification     
def kBinary1(t) :
    return -2/(3 * t**3)
def kBinary2(t) :
    return -(np.sin(t) + t * np.cos(t))/ (t*np.sin(t))**2
def kBinary3(t) :
    return - ((t+1) *np.sin(t) + t* np.cos(t))/(np.exp(t) * (t*np.sin(t))**2)


# function for the Binary Interaction MMS verification 
def BinaryVerification():
    print("---------- Choose the function for MMS verification of Binary Interaction solver: ----------")
    print("[0] F0 : 3 * t²")
    print("[1] F1 : t * sin(t)")
    print("[2] F2 : t * e^t * sin(t)")
    functionId = input("Enter the chosen function (0, 1, 2) = ")
    functionId = int(functionId)

    print("---------- Choose the mode of Binary Interaction solver: ----------")
    print("[0] old model")
    print("[1] new model")
    modeid = input("Enter the chosen function (0, 1) = ")
    modeid = int(modeid)
    if modeid == 0 or modeid == 1:
        chosen_mode = modeid
    if functionId == 0:
        print("You chose option 0")
        chosen_k = kBinary1
        exact_solution = F1
    elif functionId == 1:
        print("You chose option 1")
        chosen_k = kBinary2
        exact_solution = F2
    elif functionId == 2:
        print("You chose option 2")
        chosen_k = kBinary3
        exact_solution = F3
    else:
        print(f"{functionId} is not a valid choice")
        exit()
    # execution fo the verification
    verificationBinaryInteraction.main(chosen_k , exact_solution ,chosen_mode)

if __name__ == "__main__":
    print("---------- Choose a solver :----------")
    print("[0] Integrator")
    print("[1] Decay ")
    print("[2] Binary Interaction")
    print("[3] Limited Growth ")
    print("[4] All")
    # input reader
    solverId = input("Enter the chosen function (0, 1, 2, 3 ,4 ) = ")
    solverId = int(solverId) # conversion to a int 

    if solverId == 0:
        IntegratorVerification()
    elif solverId ==1 : 
        DecayVerification()
    elif solverId ==2 : 
        BinaryVerification()
    elif solverId ==3 : 
        LimitedGrowthVerification()
    elif solverId ==4 : 
        IntegratorVerification()
        DecayVerification()
        BinaryVerification()
        LimitedGrowthVerification()
    else :
        print(f'{solverId} is not a valid solver')
        exit()

