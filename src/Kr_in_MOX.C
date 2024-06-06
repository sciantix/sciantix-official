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

#include "Kr_in_MOX.h"

void Kr_in_MOX()
{
	// Error handling
	if (matrix.empty() || input_variable.empty() || sma.find("UO2") == sma.end())
	{
		std::cerr << "Error: Required components are not initialized in " << __FILE__  << std::endl;
		return;
	}

	sciantix_system.emplace_back();
	int index = int(sciantix_system.size() - 1);
	double enrichment = matrix[index - 1].getMoxPuEnrichment();

	sciantix_system[index].setName("Kr in MOX");
	sciantix_system[index].setGasName("Kr");
	sciantix_system[index].setMatrixName("MOX");
	sciantix_system[index].setRestructuredMatrix(0);
	sciantix_system[index].setYield(0.03*(1 - enrichment) + 0.017*enrichment);
	sciantix_system[index].setRadiusInLattice(0.21e-9); //still to be replaced
	sciantix_system[index].setVolumeInLattice(matrix[sma["MOX"]].getSchottkyVolume());
	sciantix_system[index].setHenryConstant(0.0); //still to be replaced
	sciantix_system[index].setProductionRate(1); //still to be replaced
	sciantix_system[index].setFissionGasDiffusivity(int(input_variable[iv["iFGDiffusionCoefficient"]].getValue()));
	sciantix_system[index].setBubbleDiffusivity(int(input_variable[iv["iBubbleDiffusivity"]].getValue()));
	sciantix_system[index].setResolutionRate(int(input_variable[iv["iResolutionRate"]].getValue()));
	sciantix_system[index].setTrappingRate(int(input_variable[iv["iTrappingRate"]].getValue()));
	sciantix_system[index].setNucleationRate(int(input_variable[iv["iNucleationRate"]].getValue()));
}
