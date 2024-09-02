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
	if (matrix.empty() || input_variable.empty() || sma.find("SiC") == sma.end())
	{
		std::cerr << "Error: Required components are not initialized in " << __FILE__  << std::endl;
		return;
	}

	sciantix_system.emplace_back();
	int index = int(sciantix_system.size()) - 1;

	sciantix_system[index].setName("He in SiC");
	sciantix_system[index].setGasName("He");
	sciantix_system[index].setMatrixName("SiC");
	sciantix_system[index].setHeliumDiffusivity(int(input_variable[iv["iHeDiffusivity"]].getValue()));

}