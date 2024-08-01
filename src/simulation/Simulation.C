#include "Simulation.h"


Simulation& Simulation::getInstance()
{
    if (instance == nullptr)
    {
        instance = new Simulation;
    }
    return *instance;
}

void Simulation::initialize(
    int Sciantix_options[], 
    double Sciantix_history[], 
    double Sciantix_variables[], 
    double Sciantix_scaling_factors[], 
    double Sciantix_diffusion_modes[]
)
{
    setVariables(
        Sciantix_options, 
		Sciantix_history, 
		Sciantix_variables, 
		Sciantix_scaling_factors, 
		Sciantix_diffusion_modes
    );

    setGas();
    setMatrix();
    setSystem();
}


void Simulation::execute()
{
    Burnup();

	EffectiveBurnup();

	UO2Thermochemistry();

	StoichiometryDeviation(); 

	HighBurnupStructureFormation();

	HighBurnupStructurePorosity();

	GrainGrowth();

	GrainBoundarySweeping();

	GasProduction();

	GasDecay();

	IntraGranularBubbleBehaviour();

	GasDiffusion();

	GrainBoundaryMicroCracking();

	GrainBoundaryVenting();

	InterGranularBubbleBehaviour();
}

void Simulation::UO2Thermochemistry()
{
    if (!input_variable["iStoichiometryDeviation"].getValue())
        return;

    if (history_variable["Temperature"].getFinalValue() < 1000.0 || sciantix_variable["Gap oxygen partial pressure"].getFinalValue() == 0)
        sciantix_variable["Equilibrium stoichiometry deviation"].setFinalValue(0.0);

    else
        sciantix_variable["Equilibrium stoichiometry deviation"].setFinalValue(
            solver.NewtonBlackburn(
                model["UO2 thermochemistry"].getParameter()));
}