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

#include "He_in_SiC.h"

void He_in_SiC()
{
	// Error handling
	if (shell_system.empty() || input_variable.empty() || she.find("SiC") == she.end())
	{
		std::cerr << "Error: Required components are not initialized in " << __FILE__  << std::endl;
		return;
	}

	shell_system.emplace_back();
	int index = int(shell_system.size()) - 1;

	shell_system[index].setName("He in SiC");
	shell_system[index].setGasName("He");
	shell_system[index].setMatrixName("SiC");
	shell_system[index].setHeliumDiffusivity(int(input_variable[iv["iHeDiffusivity"]].getValue()));

}