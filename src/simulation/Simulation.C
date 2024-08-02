#include "Simulation.h"

Simulation* Simulation::instance = nullptr;


Simulation* Simulation::getInstance()
{
    if (instance == nullptr)
    {
        instance = new Simulation;
    }
    return instance;
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

    std::cout << "variable done" << std::endl;
    setGas();
    std::cout << "gas done" << std::endl;
    setMatrix();
    std::cout << "matrix done" << std::endl;
    setSystem();
    std::cout << "system done" << std::endl;
}


void Simulation::execute()
{
    Burnup();

	EffectiveBurnup();

	UO2Thermochemistry();

	StoichiometryDeviation(); 

	// HighBurnupStructureFormation();

	// HighBurnupStructurePorosity();

	// GrainGrowth();

	// GrainBoundarySweeping();

	// GasProduction();

	// GasDecay();

	// IntraGranularBubbleBehaviour();

	// GasDiffusion();

	// GrainBoundaryMicroCracking();

	// GrainBoundaryVenting();

	// InterGranularBubbleBehaviour();
    // std::cout << sciantix_variable["Burnup"].getInitialValue() << std::endl;

}