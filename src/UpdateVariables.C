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

#include "UpdateVariables.h"

/// UpdateVariables

void UpdateVariables(double Sciantix_variables[], double Sciantix_diffusion_modes[])
{
	Sciantix_variables[1] = sciantix_variable[sv["Xe gap"]].getFinalValue();
	Sciantix_variables[2] = sciantix_variable[sv["Xe decayed"]].getFinalValue();
	Sciantix_variables[3] = sciantix_variable[sv["Xe released"]].getFinalValue();
	Sciantix_variables[4] = sciantix_variable[sv["Release to coolant"]].getFinalValue();
	Sciantix_variables[6] = sciantix_variable[sv["Gap pressure"]].getFinalValue();
	Sciantix_variables[7] = sciantix_variable[sv["Gap volume"]].getFinalValue();
	Sciantix_variables[9] = sciantix_variable[sv["H gap"]].getFinalValue();
	Sciantix_variables[11] = sciantix_variable[sv["Gas in gap"]].getFinalValue();
}
