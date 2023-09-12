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

#include "He_in_UO2.h"

void He_in_UO2()
{

	/**
	 * @brief This function defines the sciantix_system *Helium in UO<sub>2</sub>* and sets its attributes.
	 * 
	 */

	sciantix_system.emplace_back();
	int index = int(sciantix_system.size()) - 1;

	sciantix_system[index].setName("He in UO2");
	sciantix_system[index].setGasName("He");
	sciantix_system[index].setYield(0.0022); // from ternary fissions
	sciantix_system[index].setRadiusInLattice(4.73e-11);
	sciantix_system[index].setVolumeInLattice(matrix[0].getOIS());
	sciantix_system[index].setHeliumDiffusivity(int(input_variable[iv["iHeDiffusivity"]].getValue()));
	sciantix_system[index].setResolutionRate(int(input_variable[iv["iResolutionRate"]].getValue()));
	sciantix_system[index].setTrappingRate(int(input_variable[iv["iTrappingRate"]].getValue()));
	sciantix_system[index].setNucleationRate(int(input_variable[iv["iNucleationRate"]].getValue()));
	sciantix_system[index].setHenryConstant(4.1e+18 * exp(-7543.5 / history_variable[hv["Temperature"]].getFinalValue())); /// The Henry's constant for helium in UO<sub>2</sub>-single crystal samples is set from best estimate correlation after @ref *L. Cognini et al. Nuclear Engineering and Design 340 (2018) 240–244*. This correlation is valid in the temperature range 1073-1773 K.
	sciantix_system[index].setProductionRate(int(input_variable[iv["iHeliumProductionRate"]].getValue()));
	sciantix_system[index].setBubbleDiffusivity(int(input_variable[iv["iBubbleDiffusivity"]].getValue()));
}
