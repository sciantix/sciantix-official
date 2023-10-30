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

#include "UO2.h"

void UO2()
{
	/**
	 * @brief This routine defines the physical proprieties of the matrix UO2.
	 * 
	 */

	matrix.emplace_back();
	int index = int(matrix.size()) - 1;

	matrix[index].setName("UO2");
	matrix[index].setRef("\n\t");
	matrix[index].setTheoreticalDensity(10970.0); // (kg/m3)
	matrix[index].setGrainBoundaryMobility(int(input_variable[iv["iGrainGrowth"]].getValue()));
	matrix[index].setSurfaceTension(0.7); // (N/m)
	matrix[index].setFFinfluenceRadius(1.0e-9); // (m)
	matrix[index].setFFrange(6.0e-6); // (m)
	matrix[index].setSchottkyVolume(4.09e-29);
	matrix[index].setOIS(7.8e-30); // (m3)
	matrix[index].setSemidihedralAngle(0.872664626); // (rad)
	matrix[index].setGrainBoundaryThickness(5.0e-10); // (m)
	matrix[index].setLenticularShapeFactor(0.168610764);
	matrix[index].setGrainRadius(sciantix_variable[sv["Grain radius"]].getFinalValue()); // (m)
	matrix[index].setHealingTemperatureThreshold(1273.5); // K
	matrix[index].setGrainBoundaryVacancyDiffusivity(int(input_variable[iv["iGrainBoundaryVacancyDiffusivity"]].getValue())); // (m2/s)
}