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

	
	if (history_variable[hv["Time"]].getFinalValue()<history_variable[hv["Defect time"]].getFinalValue())
	{

		sciantix_simulation.GasInGapIntact();
		UpdateVariables(Sciantix_variables, Sciantix_diffusion_modes);

		sciantix_simulation.UpdateGasGap();
		UpdateVariables(Sciantix_variables, Sciantix_diffusion_modes);

		sciantix_simulation.PressureEvolution();
		UpdateVariables(Sciantix_variables, Sciantix_diffusion_modes);

		Output();

		history_variable.clear();
		sciantix_variable.clear();
		physics_variable.clear();
		model.clear();
		gas.clear();
	}
	else
	{
		sciantix_variable[sv["Gap pressure"]].setFinalValue(13.0);
		UpdateVariables(Sciantix_variables, Sciantix_diffusion_modes);

		sciantix_variable[sv["Water gap"]].setFinalValue((13.0-sciantix_variable[sv["Non condensable gases partial pressure"]].getFinalValue())*1e06/(PhysicsConstants::boltzmann_constant*history_variable[hv["Temperature"]].getFinalValue()));
		UpdateVariables(Sciantix_variables, Sciantix_diffusion_modes);

		sciantix_simulation.PressureEvolution();
		UpdateVariables(Sciantix_variables, Sciantix_diffusion_modes);

		// std::cout << "pression calculée" << std::endl;
		// std::cout << (sciantix_variable[sv["H gap"]].getFinalValue()+sciantix_variable[sv["Water gap"]].getFinalValue()+sciantix_variable[sv["Gas in gap"]].getFinalValue())*PhysicsConstants::boltzmann_constant*history_variable[hv["Temperature"]].getFinalValue()*1e-06 << std::endl;
		

		
		

		
		// if (history_variable[hv["Time"]].getFinalValue() > history_variable[hv["Defect time"]].getFinalValue() && history_variable[hv["Time"]].getFinalValue() < (history_variable[hv["Defect time"]].getFinalValue()+0.6)&&(sciantix_variable[sv["Non condensable gases partial pressure"]].getFinalValue()>(sciantix_variable[sv["Gap pressure"]].getFinalValue()-12.3))&&(sciantix_variable[sv["Gas in gap"]].getFinalValue()>0))
		// {
		// 	std::cout << "gros relâchement du début" << std::endl;
		// 	std::cout << "Water partial pressure" << std::endl;
		// 	std::cout << sciantix_variable[sv["Water partial pressure"]].getFinalValue() << std::endl;
		// 	std::cout << "gaz gap" << std::endl;
		// 	std::cout << sciantix_variable[sv["Gas in gap"]].getFinalValue() << std::endl;
		// 	std::cout << "pression partielle gaz" << std::endl;
		// 	std::cout << sciantix_variable[sv["Non condensable gases partial pressure"]].getFinalValue() << std::endl;
		// 	std::cout << "pression partielle H" << std::endl;
		// 	std::cout << sciantix_variable[sv["H partial pressure"]].getFinalValue() << std::endl;

		// 	sciantix_simulation.Release();
		// 	UpdateVariables(Sciantix_variables, Sciantix_diffusion_modes);

		// 	sciantix_simulation.UpdateGasGap();
		// 	UpdateVariables(Sciantix_variables, Sciantix_diffusion_modes);
		// 	sciantix_simulation.PressureEvolution();
		// 	UpdateVariables(Sciantix_variables, Sciantix_diffusion_modes);
		// 	std::cout << "gaz gap après gros relachement" << std::endl;
		// 	std::cout << sciantix_variable[sv["Gas in gap"]].getFinalValue() << std::endl;

		// 	// sciantix_simulation.PressureEvolution();
		// 	// UpdateVariables(Sciantix_variables, Sciantix_diffusion_modes);
		// 	// std::cout << "pression partielle gaz après gros relachement" << std::endl;
		// 	// std::cout << sciantix_variable[sv["Non condensable gases partial pressure"]].getFinalValue() << std::endl;
		// }

		if (sciantix_variable[sv["Water gap"]].getFinalValue()>sciantix_variable[sv["H production rate"]].getFinalValue()*physics_variable[pv["Time step"]].getFinalValue())
		{
			std::cout << "radiolyse" << std::endl;
			sciantix_simulation.CoolantRadiolysis();
			UpdateVariables(Sciantix_variables, Sciantix_diffusion_modes);
		}

		// else if (sciantix_variable[sv["Water gap"]].getFinalValue()<sciantix_variable[sv["H production rate"]].getFinalValue()*physics_variable[pv["Time step"]].getFinalValue())
		// {
		// 	sciantix_variable[sv["H gap"]].setFinalValue(
		// 		sciantix_variable[sv["Water gap"]].getFinalValue()
		// 	);
		// 	sciantix_variable[sv["Water gap"]].setFinalValue(
		// 		0
		// 	);
		// }
		
		sciantix_simulation.UpdateGasGap();
		UpdateVariables(Sciantix_variables, Sciantix_diffusion_modes);

		sciantix_simulation.PressureEvolution();
		UpdateVariables(Sciantix_variables, Sciantix_diffusion_modes);

		
		

		if (sciantix_variable[sv["Non condensable gases partial pressure"]].getFinalValue()>(sciantix_variable[sv["Gap pressure"]].getFinalValue()-9.45)) //if the limit pressure is reached gas can be released
		{
			sciantix_simulation.ReleaseRateCoefficient();
			sciantix_simulation.GasInGapFailed();
			UpdateVariables(Sciantix_variables, Sciantix_diffusion_modes);
			sciantix_simulation.GasInCoolantWhenRelease();
			UpdateVariables(Sciantix_variables, Sciantix_diffusion_modes);
			std::cout << "relache" << std::endl;
		}
		else // if not, gas accumulate in the gap and decay in the coolant
		{
			std::cout << "accumule" << std::endl;
			sciantix_simulation.GasInGapIntact();
			UpdateVariables(Sciantix_variables, Sciantix_diffusion_modes);
			sciantix_simulation.GasInCoolant();
			UpdateVariables(Sciantix_variables, Sciantix_diffusion_modes);
		}

		sciantix_simulation.UpdateGasGap();
		UpdateVariables(Sciantix_variables, Sciantix_diffusion_modes);
		
		

		sciantix_simulation.PressureEvolution();
		UpdateVariables(Sciantix_variables, Sciantix_diffusion_modes);
		
		sciantix_variable[sv["Gap pressure"]].setFinalValue(13.0);
		UpdateVariables(Sciantix_variables, Sciantix_diffusion_modes);
		
		sciantix_variable[sv["Water gap"]].setFinalValue((13.0-sciantix_variable[sv["Non condensable gases partial pressure"]].getFinalValue())*1e06/(PhysicsConstants::boltzmann_constant*history_variable[hv["Temperature"]].getFinalValue()));
		UpdateVariables(Sciantix_variables, Sciantix_diffusion_modes);
		
		sciantix_simulation.PressureEvolution();
		UpdateVariables(Sciantix_variables, Sciantix_diffusion_modes);
		

		//sciantix_variable[sv["Water gap"]].setFinalValue(((13- sciantix_variable[sv["Non condensable gases partial pressure"]].getFinalValue())*1e06)/(PhysicsConstants::boltzmann_constant*history_variable[hv["Temperature"]].getFinalValue()));

		// if (sciantix_variable[sv["Gap pressure"]].getFinalValue()<13.0)
		// {
		// 	std::cout << "pas assez de pression" << std::endl;

		// 	sciantix_variable[sv["Water gap"]].setFinalValue(sciantix_variable[sv["Water gap"]].getFinalValue()+((13.0-sciantix_variable[sv["Gap pressure"]].getFinalValue())*1e06)/(PhysicsConstants::boltzmann_constant*history_variable[hv["Temperature"]].getFinalValue()));

		// }
		// else
		// {

		// 	std::cout << "autre chose" << std::endl;
		// 	sciantix_variable[sv["Water gap"]].setFinalValue((13- sciantix_variable[sv["Non condensable gases partial pressure"]].getFinalValue()*1e06)/(PhysicsConstants::boltzmann_constant*history_variable[hv["Temperature"]].getFinalValue()));
		// }
		

		
		
		

		

		Output();

		history_variable.clear();
		sciantix_variable.clear();
		sciantix_system.clear();
		physics_variable.clear();
		model.clear();
		gas.clear();
	}
	
	// sciantix_simulation.SheathTemperatureUpdate();
	// MapModel();
	
	//sciantix_simulation.GasDecay();

	// sciantix_simulation.GasInCoolant();
	// UpdateVariables(Sciantix_variables, Sciantix_diffusion_modes);

	// sciantix_simulation.CoolantRadiolysis();
	// UpdateVariables(Sciantix_variables, Sciantix_diffusion_modes);
	
	// sciantix_simulation.ZircaloyOxidation();
	// UpdateVariables(Sciantix_variables, Sciantix_diffusion_modes);
	

	
	

	// sciantix_simulation.CoolantRadiolysis();
	// sciantix_simulation.ZircaloyOxidation();
	// sciantix_simulation.PressureEvolution();
	// sciantix_simulation.GasDecay();

	// FiguresOfMerit();
}
