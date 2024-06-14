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
	SetVariables(Sciantix_options, Sciantix_history, Sciantix_variables, Sciantix_scaling_factors, Sciantix_diffusion_modes);

	SetGas();
	SetSystem();

	// std::cout << "egalite" << std::endl;

	Simulation sciantix_simulation;
	
	// sciantix_simulation.SheathTemperatureUpdate();
	// MapModel();

	sciantix_simulation.GasInGap();
	UpdateVariables(Sciantix_variables, Sciantix_diffusion_modes);
	//sciantix_simulation.GasDecay();

	sciantix_simulation.GasInCoolant();
	std::cout << sciantix_variable[sv["Xe released"]].getFinalValue() << std::endl;
	// sciantix_simulation.CoolantRadiolysis();
	// sciantix_simulation.ZircaloyOxidation();
	// sciantix_simulation.PressureEvolution();
	// sciantix_simulation.GasDecay();

	// FiguresOfMerit();

	UpdateVariables(Sciantix_variables, Sciantix_diffusion_modes);

	Output();

	history_variable.clear();
	sciantix_variable.clear();
	physics_variable.clear();
}
