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
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "Sciantix.h"

void Sciantix(int Sciantix_options[],
	double Sciantix_history[],
	double Sciantix_variables[],
	double Sciantix_scaling_factors[],
	double Sciantix_diffusion_modes[],
    std::string testFilePath)
{
	SetVariables(Sciantix_options, Sciantix_history, Sciantix_variables, Sciantix_scaling_factors, Sciantix_diffusion_modes);

	SetGas();

	SetMatrix();

	SetSystem();

	Simulation sciantix_simulation;

	Burnup();
	MapModel();
	sciantix_simulation.Burnup();

	EffectiveBurnup();
	MapModel();
	sciantix_simulation.EffectiveBurnup();

	EnvironmentComposition();
	MapModel();

	UO2Thermochemistry();
	MapModel();
	sciantix_simulation.UO2Thermochemistry();

	StoichiometryDeviation();
	MapModel();
	sciantix_simulation.StoichiometryDeviation(); 

	HighBurnupStructureFormation();
	MapModel();
	sciantix_simulation.HighBurnupStructureFormation();

	HighBurnupStructurePorosity();
	MapModel();
	sciantix_simulation.HighBurnupStructurePorosity();

	GrainGrowth();
	MapModel();
	sciantix_simulation.GrainGrowth();

	GrainBoundarySweeping();
	MapModel();
	sciantix_simulation.GrainBoundarySweeping();

	GasProduction();
	MapModel();
	sciantix_simulation.GasProduction();

	sciantix_simulation.GasDecay();

	IntraGranularBubbleEvolution();
	MapModel();
	sciantix_simulation.IntraGranularBubbleBehaviour();

	GasDiffusion();
	MapModel();
	sciantix_simulation.GasDiffusion();

	GrainBoundaryMicroCracking();
	MapModel();
	sciantix_simulation.GrainBoundaryMicroCracking();

	GrainBoundaryVenting();
	MapModel();
	sciantix_simulation.GrainBoundaryVenting();

	InterGranularBubbleEvolution();
	MapModel();
	sciantix_simulation.InterGranularBubbleBehaviour();

	FiguresOfMerit();

	UpdateVariables(Sciantix_variables, Sciantix_diffusion_modes);

	Output(testFilePath);

	history_variable.clear();
	sciantix_variable.clear();
	sciantix_system.clear();
	physics_variable.clear();
	model.clear();
	material.clear();
	gas.clear();
	matrix.clear();
}
