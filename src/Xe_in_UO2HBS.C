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

#include "Xe_in_UO2HBS.h"

void Xe_in_UO2HBS()
{	
	// Error handling
    if (matrix.empty() || input_variable.empty() || sma.find("UO2HBS") == sma.end())
    {
        std::cerr << "Error: Required components are not initialized in " << __FILE__  << std::endl;
        return;
    }

	sciantix_system.emplace_back();
	int index = int(sciantix_system.size()) - 1;

	sciantix_system[index].setName("Xe in UO2HBS");
	sciantix_system[index].setGasName("Xe");
	sciantix_system[index].setMatrixName("UO2HBS");
	sciantix_system[index].setRestructuredMatrix(1);
	sciantix_system[index].setYield(0.24);
	sciantix_system[index].setRadiusInLattice(0.21e-9);
	sciantix_system[index].setVolumeInLattice(matrix[sma["UO2HBS"]].getSchottkyVolume());
	sciantix_system[index].setHenryConstant(0.0);
	sciantix_system[index].setProductionRate(4);
	sciantix_system[index].setFissionGasDiffusivity(4);
	sciantix_system[index].setBubbleDiffusivity(0);
	sciantix_system[index].setResolutionRate(99);
	sciantix_system[index].setTrappingRate(99);
	sciantix_system[index].setNucleationRate(99);
}

