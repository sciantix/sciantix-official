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
//  Year: 2023                                                                      //
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
			matrices.push(UO2HBS(matrices, sciantix_variable, history_variable, input_variable));

			break;
		}

		default:
			ErrorMessages::Switch(__FILE__, "iFuelMatrix", int(input_variable["iFuelMatrix"].getValue()));
			break;
	}
}

Matrix UO2(SciantixArray<Matrix> &matrices, SciantixArray<SciantixVariable> &sciantix_variable, 
	SciantixArray<SciantixVariable> &history_variable, SciantixArray<InputVariable> &input_variable)
{
	Matrix matrix;

	matrix.setName("UO2");
	matrix.setRef("\n\t");
	matrix.setTheoreticalDensity(10960.0); // (kg/m3)
	matrix.setLatticeParameter(5.47e-10);
	matrix.setGrainBoundaryMobility(int(input_variable["iGrainGrowth"].getValue()), history_variable);
	matrix.setSurfaceTension(0.7); // (N/m)
	matrix.setFFinfluenceRadius(1.0e-9); // (m)
	matrix.setFFrange(6.0e-6); // (m)
	matrix.setSchottkyVolume(4.09e-29); // (m3)
	matrix.setOIS(7.8e-30); // (m3)
	matrix.setSemidihedralAngle(0.872664626); // (rad)
	matrix.setGrainBoundaryThickness(5.0e-10); // (m)
	matrix.setLenticularShapeFactor(0.168610764);
	matrix.setGrainRadius(sciantix_variable["Grain radius"].getFinalValue()); // (m)
	matrix.setHealingTemperatureThreshold(1273.15); // K
	matrix.setGrainBoundaryVacancyDiffusivity(int(input_variable["iGrainBoundaryVacancyDiffusivity"].getValue()), history_variable); // (m2/s)
	matrix.setPoreNucleationRate(sciantix_variable);
	matrix.setPoreResolutionRate(sciantix_variable, history_variable);
	matrix.setPoreTrappingRate(matrices, sciantix_variable);

    return matrix;
}

Matrix UO2HBS(SciantixArray<Matrix> &matrices, SciantixArray<SciantixVariable> &sciantix_variable, 
	SciantixArray<SciantixVariable> &history_variable, SciantixArray<InputVariable> &input_variable)
{
	Matrix matrix;
	
	matrix.setName("UO2HBS");
	matrix.setRef("\n\t");
	matrix.setTheoreticalDensity(10960.0); // (kg/m3)
	matrix.setLatticeParameter(5.47e-10);
	matrix.setGrainBoundaryMobility(0, history_variable);
	matrix.setSurfaceTension(0.7); // (N/m)
	matrix.setFFinfluenceRadius(1.0e-9); // (m)
	matrix.setFFrange(6.0e-6); // (m)
	matrix.setSchottkyVolume(4.09e-29);
	matrix.setOIS(7.8e-30);
	matrix.setSemidihedralAngle(0.0);
	matrix.setGrainBoundaryThickness(0.0);
	matrix.setLenticularShapeFactor(0.168610764);
	matrix.setGrainRadius(150e-9); // (m)
	matrix.setHealingTemperatureThreshold(1273.15); // K
	matrix.setGrainBoundaryVacancyDiffusivity(5, history_variable); // (m2/s)
	matrix.setPoreNucleationRate(sciantix_variable);
	matrix.setPoreResolutionRate(sciantix_variable, history_variable);
	matrix.setPoreTrappingRate(matrices, sciantix_variable);

	return matrix;
}