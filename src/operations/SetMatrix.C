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
    matrix_.setGrainBoundaryVacancyDiffusivity(int(input_variable["iGrainBoundaryVacancyDiffusivity"].getValue()), history_variable, sciantix_variable); // (m2/s)
    matrix_.setPoreNucleationRate(sciantix_variable);
    matrix_.setPoreResolutionRate(sciantix_variable, history_variable);
    matrix_.setPoreTrappingRate(matrices, sciantix_variable);

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
    matrix_.setGrainBoundaryVacancyDiffusivity(5, history_variable, sciantix_variable); // (m2/s)
    matrix_.setPoreNucleationRate(sciantix_variable);
    matrix_.setPoreResolutionRate(sciantix_variable, history_variable);
    matrix_.setPoreTrappingRate(matrices, sciantix_variable);

    return matrix_;
}