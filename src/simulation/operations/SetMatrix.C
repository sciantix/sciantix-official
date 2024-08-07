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

#include "SetMatrix.h"

void Simulation::setMatrix()
{
  /**
   * @brief This routine defines the available options for fuel matrices and their properties.
   * 
   */
	switch (int(input_variable["iFuelMatrix"].getValue()))
	{
		case 0: 
		{
			matrices.push(UO2(matrices, sciantix_variable, history_variable, input_variable));

			break;
		}

		case 1: 
		{
			matrices.push(UO2(matrices, sciantix_variable, history_variable, input_variable));
			// matrices.push(UO2HBS(matrices, sciantix_variable, history_variable, input_variable));

			break;
		}
		
		default:
			ErrorMessages::Switch(__FILE__, "iFuelMatrix", int(input_variable["iFuelMatrix"].getValue()));
			break;
	}
}

Matrix UO2(SciantixArray<Matrix> matrices, SciantixArray<SciantixVariable> sciantix_variable, 
	SciantixArray<SciantixVariable> history_variable, SciantixArray<InputVariable> input_variable)
{
	Matrix mat;

	mat.setName("UO2");
	mat.setRef("\n\t");
	mat.setTheoreticalDensity(10960.0); // (kg/m3)
	mat.setLatticeParameter(5.47e-10);
	mat.setGrainBoundaryMobility(int(input_variable["iGrainGrowth"].getValue()), history_variable);
	mat.setSurfaceTension(0.7); // (N/m)
	mat.setFFinfluenceRadius(1.0e-9); // (m)
	mat.setFFrange(6.0e-6); // (m)
	mat.setSchottkyVolume(4.09e-29); // (m3)
	mat.setOIS(7.8e-30); // (m3)
	mat.setSemidihedralAngle(0.872664626); // (rad)
	mat.setGrainBoundaryThickness(5.0e-10); // (m)
	mat.setLenticularShapeFactor(0.168610764);
	mat.setGrainRadius(sciantix_variable["Grain radius"].getFinalValue()); // (m)
	mat.setHealingTemperatureThreshold(1273.15); // K
	mat.setGrainBoundaryVacancyDiffusivity(int(input_variable["iGrainBoundaryVacancyDiffusivity"].getValue()), history_variable); // (m2/s)
	mat.setPoreNucleationRate(sciantix_variable);
	mat.setPoreResolutionRate(sciantix_variable, history_variable);
	mat.setPoreTrappingRate(matrices, sciantix_variable);

    return mat;
}


Matrix UO2HBS(SciantixArray<Matrix> matrices, SciantixArray<SciantixVariable> sciantix_variable, 
	SciantixArray<SciantixVariable> history_variable, SciantixArray<InputVariable> input_variable)
{
	Matrix mat;
	
	mat.setName("UO2HBS");
	mat.setRef("\n\t");
	mat.setTheoreticalDensity(10960.0); // (kg/m3)
	mat.setLatticeParameter(5.47e-10);
	mat.setGrainBoundaryMobility(0, history_variable);
	mat.setSurfaceTension(0.7); // (N/m)
	mat.setFFinfluenceRadius(1.0e-9); // (m)
	mat.setFFrange(6.0e-6); // (m)
	mat.setSchottkyVolume(4.09e-29);
	mat.setOIS(7.8e-30);
	mat.setSemidihedralAngle(0.0);
	mat.setGrainBoundaryThickness(0.0);
	mat.setLenticularShapeFactor(0.168610764);
	mat.setGrainRadius(150e-9); // (m)
	mat.setHealingTemperatureThreshold(1273.15); // K
	mat.setGrainBoundaryVacancyDiffusivity(5, history_variable); // (m2/s)
	mat.setPoreNucleationRate(sciantix_variable);
	mat.setPoreResolutionRate(sciantix_variable, history_variable);
	mat.setPoreTrappingRate(matrices, sciantix_variable);

	return mat;
}