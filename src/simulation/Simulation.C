//////////////////////////////////////////////////////////////////////////////////////
//       _______.  ______  __       ___      .__   __. .___________. __  ___   ___  //
//      /       | /      ||  |     /   \     |  \ |  | |           ||  | \  \ /  /  //
//     |   (----`|  ,----'|  |    /  ^  \    |   \|  | `---|  |----`|  |  \  V  /   //
//      \   \    |  |     |  |   /  /_\  \   |  . `  |     |  |     |  |   >   <    //
//  .----)   |   |  `----.|  |  /  _____  \  |  |\   |     |  |     |  |  /  .  \   //
//  |_______/     \______||__| /__/     \__\ |__| \__|     |__|     |__| /__/ \__\  //
//                                                                                  //
//  Originally developed by D. Pizzocri & T. Barani                                 //
//                                                                                  //
//  Version: 2.0                                                                    //
//  Year: 2022                                                                      //
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//  Contributors: F. Bastien                                                        //
//////////////////////////////////////////////////////////////////////////////////////

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

    setGas();
    setMatrix();
    setSystem();
}


void Simulation::execute()
{
    Burnup();

	EffectiveBurnup();

	GapPartialPressure(); // atm partial pressure

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