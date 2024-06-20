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

#include "Xe_in_UO2.h"

void Xe_in_UO2()
{
	// Error handling
	if (matrix.empty() || input_variable.empty() || sma.find("UO2HBS") == sma.end())
	{
		std::cerr << "Error: Required components are not initialized in " << __FILE__  << std::endl;
		return;
	}

	sciantix_system.emplace_back();
	int index = int(sciantix_system.size() - 1);

	sciantix_system[index].setName("Xe in UO2");
	sciantix_system[index].setGasName("Xe");
	sciantix_system[index].setMatrixName("UO2");
	sciantix_system[index].setRestructuredMatrix(0);
	sciantix_system[index].setYield(0.24);
	sciantix_system[index].setRadiusInLattice(0.21e-9); // (m), from experimental data, assumed equal for Xe and Kr
	sciantix_system[index].setVolumeInLattice(matrix[sma["UO2"]].getSchottkyVolume());
	sciantix_system[index].setHenryConstant(0.0);
	sciantix_system[index].setProductionRate(1);
	sciantix_system[index].setFissionGasDiffusivity(int(input_variable[iv["iFGDiffusionCoefficient"]].getValue()));
	sciantix_system[index].setBubbleDiffusivity(int(input_variable[iv["iBubbleDiffusivity"]].getValue()));
	sciantix_system[index].setResolutionRate(int(input_variable[iv["iResolutionRate"]].getValue()));
	sciantix_system[index].setTrappingRate(int(input_variable[iv["iTrappingRate"]].getValue()));
	sciantix_system[index].setNucleationRate(int(input_variable[iv["iNucleationRate"]].getValue()));
}
