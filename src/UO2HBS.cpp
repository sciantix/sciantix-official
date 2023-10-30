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
//  Authors: D. Pizzocri, G. Zullo, A. Magni, E. Redaelli                           //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "UO2HBS.h"

void UO2HBS()
{
	/**
	 * @brief This routine defines the physical proprieties of the matrix UO2HBS.
	 * UO2HBS: UO2 in case of 100% High Burnup Structure (HBS) conditions.
	 * 
	 */

	matrix.emplace_back();
	int index = int(matrix.size()) - 1;
	
	matrix[index].setName("UO2HBS");
	matrix[index].setRef("\n\t");
	matrix[index].setTheoreticalDensity(10970.0); // (kg/m3)
	matrix[index].setGrainBoundaryMobility(0);
	matrix[index].setSurfaceTension(0.7); // (N/m)
	matrix[index].setFFinfluenceRadius(1.0e-9); // (m)
	matrix[index].setFFrange(6.0e-6); // (m)
	matrix[index].setSchottkyVolume(4.09e-29);
	matrix[index].setOIS(7.8e-30);
	matrix[index].setSemidihedralAngle(0.0);
	matrix[index].setGrainBoundaryThickness(0.0);
	matrix[index].setLenticularShapeFactor(0.168610764);
	matrix[index].setGrainRadius(150e-9); // (m)
	matrix[index].setHealingTemperatureThreshold(1273.5); // K
	matrix[index].setGrainBoundaryVacancyDiffusivity(0); // (m2/s)
}