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

#include "EnvironmentComposition.h"

void EnvironmentComposition()
{
	if (!input_variable[iv["iStoichiometryDeviation"]].getValue()) return;

	model.emplace_back();
	int model_index = int(model.size()) - 1;
	model[model_index].setName("Environment composition");

	// Calculate equilibrium constant using law of mass action for water vapor decomposition
	// @param equilibrium_constant Equilibrium constant for water vapor decomposition (atm)
	// @ref Morel et al., CEA, Report NT/DTP/SECC no. DR94-55 (1994)
	double equilibrium_constant = exp(-25300.0 / history_variable[hv["Temperature"]].getFinalValue() + 4.64 + 1.04 * (0.0007 * history_variable[hv["Temperature"]].getFinalValue() - 0.2));

	// Calculate gap oxygen partial pressure using the calculated equilibrium constant and steam pressure
	// @param steam_pressure Steam pressure (atm)
	// @param gap_oxygen_partial_pressure Gap oxygen partial pressure (atm)
	double steam_pressure = history_variable[hv["Steam pressure"]].getFinalValue();
	double gap_oxygen_partial_pressure = pow(pow(equilibrium_constant, 2) * pow(steam_pressure, 2) / 4, 1.0 / 3.0);

	sciantix_variable[sv["Gap oxygen partial pressure"]].setFinalValue(gap_oxygen_partial_pressure); // (atm)

	std::string reference = "Lewis et al. JNM 227 (1995) 83-109, D.R. Olander, Nucl. Technol. 74 (1986) 215.";
	model[model_index].setRef(reference);
}
