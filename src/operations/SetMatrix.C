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
//  Version: 2.1                                                                    //
//  Year: 2024                                                                      //
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "SetMatrix.h"

void Simulation::setMatrix()
{
    switch (int(input_variable["iFuelMatrix"].getValue()))
    {
        case 0: 
        {
            matrices.push(UO2(matrices, sciantix_variable, history_variable, input_variable, scaling_factors));
            break;
        }

        case 1: 
        {
            matrices.push(UO2(matrices, sciantix_variable, history_variable, input_variable, scaling_factors));
            matrices.push(UO2HBS(matrices, sciantix_variable, history_variable, input_variable));
            break;
        }

        default:
            ErrorMessages::Switch(__FILE__, "iFuelMatrix", int(input_variable["iFuelMatrix"].getValue()));
            break;
    }
}

Matrix UO2(SciantixArray<Matrix> &matrices, SciantixArray<SciantixVariable> &sciantix_variable, 
    SciantixArray<SciantixVariable> &history_variable, SciantixArray<InputVariable> &input_variable, SciantixArray<InputVariable> &scaling_factor)
{
    Matrix matrix_;

    matrix_.setName("UO2");
    matrix_.setRef("\n\t");
    matrix_.setTheoreticalDensity(10960.0); // (kg/m3)
    matrix_.setLatticeParameter(5.47e-10);
    matrix_.setGrainBoundaryMobility(int(input_variable["iGrainGrowth"].getValue()), history_variable);
    matrix_.setSurfaceTension(0.7); // (N/m)
    matrix_.setFissionFragmentInfluenceRadius(1.0e-9); // (m)
    matrix_.setFissionFragmentRange(6.0e-6); // (m)
    matrix_.setSchottkyVolume(4.09e-29); // (m3)
    matrix_.setOctahedralInterstitialSite(7.8e-30); // (m3)
    matrix_.setSemidihedralAngle(0.872664626); // (rad)
    matrix_.setGrainBoundaryThickness(5.0e-10); // (m)
    matrix_.setLenticularShapeFactor(0.168610764);
    matrix_.setGrainRadius(sciantix_variable["Grain radius"].getFinalValue()); // (m)
    matrix_.setHealingTemperatureThreshold(1273.15); // K
    matrix_.setGrainBoundaryVacancyDiffusivity(int(input_variable["iGrainBoundaryVacancyDiffusivity"].getValue()), history_variable); // (m2/s)
    matrix_.setPoreNucleationRate(sciantix_variable);
    matrix_.setPoreResolutionRate(sciantix_variable, history_variable);
    matrix_.setPoreTrappingRate(matrices, sciantix_variable);
    matrix_.setChromiumSolubility(0); // (weight%/UO2)	
	matrix_.setChromiaSolubility(0); // (weight%/UO2)	
	matrix_.setChromiumSolution(0); // (at/m3)
	matrix_.setChromiumPrecipitate(0); // (at/m3)
	matrix_.setChromiaSolution(0); // (at/m3)
	matrix_.setChromiaPrecipitate(0); // (at/m3)

    // Mechanical properties
    
	// Elastic modulus
    matrix_.setElasticModulus(2.237e5 * (1 - 2.6 * (1 - sciantix_variable["Fuel density"].getFinalValue() / 10960)) * (1 - 1.394e-4 * (history_variable["Temperature"].getFinalValue()-273-20)) * (1 - 0.1506 * (1 - exp(-0.035*sciantix_variable["Burnup"].getFinalValue())))); // (MPa) TU
	if ((1 - sciantix_variable["Fuel density"].getFinalValue() / 10960)>=0.2){
		std::cout<<"WARNING: elastic modulus correlation used outside the validity range for fuel porosity (P<0.2)"<<std::endl;
		std::cout<<"Porosity P = "<<(1 - sciantix_variable["Fuel density"].getFinalValue() / 10960)<<std::endl;
	}

	// Poisson ratio
	matrix_.setPoissonRatio(0.32); // (/) TU

	// Grain boundary energy 
	//
	// from surface tension
	//matrix[index].setGrainBoundaryFractureEnergy(((2*0.6*cos(0.872664626)))); // (N/m)  surface tension 	
	//
	// from inverse calibration
	//matrix[index].setGrainBoundaryFractureEnergy(4e-3); // (J/m2) @Jernkvist2019
	//
	// from mechanical testing
	matrix_.setGrainBoundaryFractureEnergy(2*scaling_factor["Dummy"].getValue()); // (J/m2) @Jernkvist2020

    return matrix_;
}

Matrix UO2HBS(SciantixArray<Matrix> &matrices, SciantixArray<SciantixVariable> &sciantix_variable, 
    SciantixArray<SciantixVariable> &history_variable, SciantixArray<InputVariable> &input_variable)
{
    Matrix matrix_;
    
    matrix_.setName("UO2HBS");
    matrix_.setRef("\n\t");
    matrix_.setTheoreticalDensity(10960.0); // (kg/m3)
    matrix_.setLatticeParameter(5.47e-10);
    matrix_.setGrainBoundaryMobility(0, history_variable);
    matrix_.setSurfaceTension(0.7); // (N/m)
    matrix_.setFissionFragmentInfluenceRadius(1.0e-9); // (m)
    matrix_.setFissionFragmentRange(6.0e-6); // (m)
    matrix_.setSchottkyVolume(4.09e-29);
    matrix_.setOctahedralInterstitialSite(7.8e-30);
    matrix_.setSemidihedralAngle(0.0);
    matrix_.setGrainBoundaryThickness(0.0);
    matrix_.setLenticularShapeFactor(0.168610764);
    matrix_.setGrainRadius(150e-9); // (m)
    matrix_.setHealingTemperatureThreshold(1273.15); // K
    matrix_.setGrainBoundaryVacancyDiffusivity(5, history_variable); // (m2/s)
    matrix_.setPoreNucleationRate(sciantix_variable);
    matrix_.setPoreResolutionRate(sciantix_variable, history_variable);
    matrix_.setPoreTrappingRate(matrices, sciantix_variable);

    return matrix_;
}
