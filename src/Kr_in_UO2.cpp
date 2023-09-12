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

#include "Kr_in_UO2.h"

void Kr_in_UO2()
{

	/**
	 * @brief This function defines the sciantix_system *Krypton in UO<sub>2</sub>* and sets its physical properties.
	 * 
	 */

	sciantix_system.emplace_back();
	int index = int(sciantix_system.size() - 1);

	sciantix_system[index].setName("Kr in UO2");
	sciantix_system[index].setGasName("Kr");
	sciantix_system[index].setYield(0.03);
	sciantix_system[index].setRadiusInLattice(0.21e-9);
	sciantix_system[index].setVolumeInLattice(matrix[0].getSchottkyVolume());
	sciantix_system[index].setHenryConstant(0.0);
	sciantix_system[index].setProductionRate(1);
	sciantix_system[index].setFissionGasDiffusivity(int(input_variable[iv["iFGDiffusionCoefficient"]].getValue()));
	sciantix_system[index].setBubbleDiffusivity(int(input_variable[iv["iBubbleDiffusivity"]].getValue()));
	sciantix_system[index].setResolutionRate(int(input_variable[iv["iResolutionRate"]].getValue()));
	sciantix_system[index].setTrappingRate(int(input_variable[iv["iTrappingRate"]].getValue()));
	sciantix_system[index].setNucleationRate(int(input_variable[iv["iNucleationRate"]].getValue()));
}
