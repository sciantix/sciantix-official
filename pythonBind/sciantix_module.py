import sys
import os

module_path = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'build', 'python'))
if module_path not in sys.path:
    sys.path.append(module_path)

# importation of the Module
import sciantixModule

def getMainVar():
    """This function fetches the values of global attributes in the C++/C code."""
    # Create an instance of the Simulation class
    sciantix_simulation = sciantixModule.Simulation.getInstance()


def Sciantix(Sciantix_options, Sciantix_history, Sciantix_variables, Sciantix_scaling_factors, Sciantix_diffusion_modes):
    """This function is the Python function of the simulation of Sciantix."""
    
    # Set variables
    sciantixModule.SetVariables(Sciantix_options, Sciantix_history, Sciantix_variables, Sciantix_scaling_factors, Sciantix_diffusion_modes)
        
    # Initialize components
    sciantixModule.SetGas()
    sciantixModule.SetMatrix()
    sciantixModule.SetSystem()

    # Create a Simulation instance
    sciantix_simulation = sciantixModule.Simulation.getInstance()
    
    # Perform simulation steps
    sciantix_simulation.Burnup()
    sciantix_simulation.EffectiveBurnup()
    sciantix_simulation.UO2Thermochemistry()
    sciantix_simulation.StoichiometryDeviation()
    sciantix_simulation.HighBurnupStructureFormation()
    sciantix_simulation.HighBurnupStructurePorosity()
    sciantix_simulation.GrainGrowth()
    sciantix_simulation.GrainBoundarySweeping()
    sciantix_simulation.GasProduction()
    sciantix_simulation.GasDecay()
    sciantix_simulation.IntraGranularBubbleBehaviour()
    sciantix_simulation.GasDiffusion()
    sciantix_simulation.GrainBoundaryMicroCracking()
    sciantix_simulation.GrainBoundaryVenting()
    sciantix_simulation.InterGranularBubbleBehaviour()

    getMainVar()