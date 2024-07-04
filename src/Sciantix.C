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
	SetGas();
	SetSystem();

	SetVariables(Sciantix_options, Sciantix_history, Sciantix_variables, Sciantix_scaling_factors, Sciantix_diffusion_modes);
	
	Simulation sciantix_simulation;
	
	// sciantix_simulation.SheathTemperatureUpdate();
	// MapModel();

	sciantix_simulation.GasInGap();
	UpdateVariables(Sciantix_variables, Sciantix_diffusion_modes);

	sciantix_simulation.UpdateGasGap();
	UpdateVariables(Sciantix_variables, Sciantix_diffusion_modes);
	
	//sciantix_simulation.GasDecay();

	// sciantix_simulation.GasInCoolant();
	// UpdateVariables(Sciantix_variables, Sciantix_diffusion_modes);

	// sciantix_simulation.CoolantRadiolysis();
	// UpdateVariables(Sciantix_variables, Sciantix_diffusion_modes);
	
	// sciantix_simulation.ZircaloyOxidation();
	// UpdateVariables(Sciantix_variables, Sciantix_diffusion_modes);

	sciantix_simulation.PressureEvolution();
	UpdateVariables(Sciantix_variables, Sciantix_diffusion_modes);

	std::cout << "pression" << std::endl;
	std::cout << sciantix_variable[sv["Gap pressure"]].getFinalValue() << std::endl;

	
	

	// sciantix_simulation.CoolantRadiolysis();
	// sciantix_simulation.ZircaloyOxidation();
	// sciantix_simulation.PressureEvolution();
	// sciantix_simulation.GasDecay();

	// FiguresOfMerit();

	Output();

	history_variable.clear();
	sciantix_variable.clear();
	physics_variable.clear();
}
