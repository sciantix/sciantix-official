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

	
	if (history_variable[hv["Time"]].getFinalValue()<=history_variable[hv["Defect time"]].getFinalValue())
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

		if (sciantix_variable[sv["Gap pressure"]].getFinalValue()<13.0)
		{
			std::cout << "pas assez de pression" << std::endl;

			sciantix_variable[sv["Water gap"]].setFinalValue(sciantix_variable[sv["Water gap"]].getFinalValue()+((13.0-sciantix_variable[sv["Gap pressure"]].getFinalValue())*1e06)/(PhysicsConstants::boltzmann_constant*history_variable[hv["Temperature"]].getFinalValue()));

		}
		else if (sciantix_variable[sv["Gap pressure"]].getFinalValue()>13.0 && sciantix_variable[sv["Non condensable gases partial pressure"]].getFinalValue()>0.0 && sciantix_variable[sv["Non condensable gases partial pressure"]].getFinalValue()>(sciantix_variable[sv["Gap pressure"]].getFinalValue()-9.45))
		{
			while (sciantix_variable[sv["Non condensable gases partial pressure"]].getFinalValue()>0.0 && sciantix_variable[sv["Non condensable gases partial pressure"]].getFinalValue()>(sciantix_variable[sv["Gap pressure"]].getFinalValue()-9.45))
			{
				std::cout << sciantix_variable[sv["Non condensable gases partial pressure"]].getFinalValue() << std::endl;
				sciantix_simulation.ReleaseRateCoefficient();

				sciantix_simulation.GasInGapFailed();
				UpdateVariables(Sciantix_variables, Sciantix_diffusion_modes);

				sciantix_simulation.GasInCoolantWhenRelease();
				UpdateVariables(Sciantix_variables, Sciantix_diffusion_modes);

				sciantix_simulation.PressureEvolution();
				UpdateVariables(Sciantix_variables, Sciantix_diffusion_modes);

			}
		}
		else
		{
			std::cout << "autre chose" << std::endl;
			sciantix_variable[sv["Water gap"]].setFinalValue(sciantix_variable[sv["Water gap"]].getFinalValue()-((sciantix_variable[sv["Gap pressure"]].getFinalValue()-13)*1e06)/(PhysicsConstants::boltzmann_constant*history_variable[hv["Temperature"]].getFinalValue()));
		}
		UpdateVariables(Sciantix_variables, Sciantix_diffusion_modes);

		sciantix_simulation.PressureEvolution();
		UpdateVariables(Sciantix_variables, Sciantix_diffusion_modes);

		sciantix_simulation.CoolantRadiolysis();
		UpdateVariables(Sciantix_variables, Sciantix_diffusion_modes);
		
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
		}
		else // if not, gas accumulate in the gap
		{
			sciantix_simulation.GasInGapIntact();
			UpdateVariables(Sciantix_variables, Sciantix_diffusion_modes);
			sciantix_simulation.GasInCoolant();
			UpdateVariables(Sciantix_variables, Sciantix_diffusion_modes);
		}

		sciantix_simulation.UpdateGasGap();
		UpdateVariables(Sciantix_variables, Sciantix_diffusion_modes);

		

		sciantix_simulation.PressureEvolution();
		UpdateVariables(Sciantix_variables, Sciantix_diffusion_modes);
		
		

		

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
