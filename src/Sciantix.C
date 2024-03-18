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
	double Sciantix_diffusion_modes[])
{
	std::cout << "beg" << std::endl;
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
	std::cout << sciantix_variable[sv["Grain radius"]].getFinalValue() << std::endl;

	HighBurnupStructureFormation();
	MapModel();
	sciantix_simulation.HighBurnupStructureFormation();
	std::cout << sciantix_variable[sv["Grain radius"]].getFinalValue() << std::endl;

	HighBurnupStructurePorosity();
	MapModel();
	sciantix_simulation.HighBurnupStructurePorosity();
	std::cout << sciantix_variable[sv["Grain radius"]].getFinalValue() << std::endl;

	GrainGrowth();
	MapModel();
	sciantix_simulation.GrainGrowth();

	GrainBoundarySweeping();
	MapModel();
	sciantix_simulation.GrainBoundarySweeping();
	std::cout << sciantix_variable[sv["Grain radius"]].getFinalValue() << std::endl;

	GasProduction();
	MapModel();
	sciantix_simulation.GasProduction();
	std::cout << sciantix_variable[sv["Grain radius"]].getFinalValue() << std::endl;

	sciantix_simulation.GasDecay();
	std::cout << sciantix_variable[sv["Grain radius"]].getFinalValue() << std::endl;

	IntraGranularBubbleEvolution();
	MapModel();
	sciantix_simulation.IntraGranularBubbleBehaviour();

	std::cout << sciantix_variable[sv["Grain radius"]].getFinalValue() << std::endl;

	GasDiffusion();
	std::cout << sciantix_variable[sv["Grain radius"]].getFinalValue() << std::endl;

	MapModel();
	sciantix_simulation.GasDiffusion();
	std::cout << sciantix_variable[sv["Grain radius"]].getFinalValue() << std::endl;

	std::cout << "cracking\n";
	GrainBoundaryMicroCracking();
	std::cout << sciantix_variable[sv["Grain radius"]].getFinalValue() << std::endl;

	MapModel();
	sciantix_simulation.GrainBoundaryMicroCracking();
	std::cout << sciantix_variable[sv["Grain radius"]].getFinalValue() << std::endl;

    // GrainBoundaryRupture();
    // MapModel();

	// GrainBoundaryVenting();
	// MapModel();
	// sciantix_simulation.GrainBoundaryVenting();

	InterGranularBubbleEvolution();
	MapModel();
	std::cout << sciantix_variable[sv["Grain radius"]].getFinalValue() << std::endl;

	sciantix_simulation.InterGranularBubbleBehaviour();
	std::cout << sciantix_variable[sv["Grain radius"]].getFinalValue() << std::endl;

	FiguresOfMerit();
	std::cout << sciantix_variable[sv["Grain radius"]].getFinalValue() << std::endl;

	UpdateVariables(Sciantix_variables, Sciantix_diffusion_modes);

	Output();

	history_variable.clear();
	sciantix_variable.clear();
	sciantix_system.clear();
	physics_variable.clear();
	model.clear();
	material.clear();
	gas.clear();
	matrix.clear();
}
