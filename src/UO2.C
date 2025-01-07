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
	matrix[index].setTheoreticalDensity(10960.0); // (kg/m3)
	matrix[index].setGrainBoundaryMobility(int(input_variable[iv["iGrainGrowth"]].getValue()));
	matrix[index].setSurfaceTension(0.6); // (N/m) Olander / Fundamental aspects...
	matrix[index].setFFinfluenceRadius(1.0e-9); // (m)
	matrix[index].setFFrange(6.0e-6); // (m)
	matrix[index].setSchottkyVolume(4.09e-29);
	matrix[index].setOIS(7.8e-30); // (m3)
	matrix[index].setSemidihedralAngle(0.872664626); // (rad)
	matrix[index].setGrainBoundaryThickness(5.0e-10); // (m)
	matrix[index].setLenticularShapeFactor(0.168610764);
	matrix[index].setGrainRadius(sciantix_variable[sv["Grain radius"]].getFinalValue()); // (m)
	matrix[index].setHealingTemperatureThreshold(1273.15); // K
	matrix[index].setGrainBoundaryVacancyDiffusivity(int(input_variable[iv["iGrainBoundaryVacancyDiffusivity"]].getValue())); // (m2/s)
	matrix[index].setPoreNucleationRate();
	matrix[index].setPoreResolutionRate();
	matrix[index].setPoreTrappingRate();

	// Mechanical properties
    
	// Elastic modulus
	//
	// MATPRO
	// matrix[index].setElasticModulus(2.334e5 * (1 - 2.752 * (1 - sciantix_variable[sv["Fuel density"]].getFinalValue() / 10960)) * (1 - 1.0915e-4 * history_variable[hv["Temperature"]].getFinalValue())); // (MPa) MATPRO (1979)
	//
	// TU
    matrix[index].setElasticModulus(2.237e5 * (1 - 2.6 * (1 - sciantix_variable[sv["Fuel density"]].getFinalValue() / 10960)) * (1 - 1.394e-4 * (history_variable[hv["Temperature"]].getFinalValue()-273-20)) * (1 - 0.1506 * (1 - exp(-0.035*sciantix_variable[sv["Burnup"]].getFinalValue())))); // (MPa) TU
	if ((1 - sciantix_variable[sv["Fuel density"]].getFinalValue() / 10960)>=0.2){
		std::cout<<"WARNING: elastic modulus correlation used outside the validity range for fuel porosity (P<0.2)"<<std::endl;
		std::cout<<"Porosity P = "<<(1 - sciantix_variable[sv["Fuel density"]].getFinalValue() / 10960)<<std::endl;
	}

	// Poisson ratio
	// 
	// MATPRO
    //matrix[index].setPoissonRatio(0.316); // (/) MATPRO (1979) 
	//
	// TU
	matrix[index].setPoissonRatio(0.32); // (/) TU

	// Grain boundary energy 
	//
	// from surface tension
	//matrix[index].setGrainBoundaryFractureEnergy(((2*0.6*cos(0.872664626)))); // (N/m)  surface tension 	
	//
	// from inverse calibration
	//matrix[index].setGrainBoundaryFractureEnergy(4e-3); // (J/m2) @Jernkvist2019
	//
	// from mechanical testing
	matrix[index].setGrainBoundaryFractureEnergy(2); // (J/m2) @Jernkvist2020
}
