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

#include "SetSystem.h"
#include "Simulation.h"

void Simulation::setSystem()
{
    switch ((int)input_variable["iFuelMatrix"].getValue())
    {
        case 0:
            sciantix_system.push(Xe_in_UO2(matrices, gas, input_variable, sciantix_variable, history_variable, scaling_factors));
            // sciantix_system.push(Kr_in_UO2(matrices, gas, input_variable, sciantix_variable, history_variable, scaling_factors));
            // sciantix_system.push(He_in_UO2(matrices, gas, input_variable, sciantix_variable, history_variable, scaling_factors));
            // sciantix_system.push(Xe133_in_UO2(matrices, gas, input_variable, sciantix_variable, history_variable, scaling_factors));
            // sciantix_system.push(Kr85m_in_UO2(matrices, gas, input_variable, sciantix_variable, history_variable, scaling_factors));
            break;

        case 1:
            sciantix_system.push(Xe_in_UO2(matrices, gas, input_variable, sciantix_variable, history_variable, scaling_factors));
            sciantix_system.push(Xe_in_UO2HBS(matrices, gas, input_variable, sciantix_variable, history_variable, scaling_factors));
            break;

        default:
            break;
    }
}

System Xe_in_UO2(SciantixArray<Matrix> &matrices, SciantixArray<Gas> &gas, SciantixArray<InputVariable> &input_variable,
    SciantixArray<SciantixVariable> &sciantix_variable, SciantixArray<SciantixVariable> &history_variable, SciantixArray<InputVariable> &scaling_factors)
{
    System system_;

    system_.setName("Xe in UO2");
    system_.setGas(gas["Xe"]);
    system_.setMatrix(matrices["UO2"]);
    system_.setRestructuredMatrix(0);
    system_.setYield(0.24);
    system_.setRadiusInLattice(0.21e-9); // (m), from experimental data, assumed equal for Xe and Kr
    system_.setVolumeInLattice(matrices["UO2"].getSchottkyVolume());
    system_.setHenryConstant(0.0);
    system_.setProductionRate(1, history_variable, input_variable, sciantix_variable, scaling_factors);
    system_.setFissionGasDiffusivity(int(input_variable["iFissionGasDiffusivity"].getValue()), sciantix_variable, history_variable, scaling_factors);
    system_.setBubbleDiffusivity(int(input_variable["iBubbleDiffusivity"].getValue()), sciantix_variable, history_variable, matrices);
    system_.setResolutionRate(int(input_variable["iResolutionRate"].getValue()), sciantix_variable, history_variable, scaling_factors, matrices);
    system_.setTrappingRate(int(input_variable["iTrappingRate"].getValue()), sciantix_variable, scaling_factors);
    system_.setNucleationRate(int(input_variable["iNucleationRate"].getValue()), history_variable, scaling_factors);

    return system_;
}

System Xe_in_UO2HBS(SciantixArray<Matrix> &matrices, SciantixArray<Gas> &gas, SciantixArray<InputVariable> &input_variable,
    SciantixArray<SciantixVariable> &sciantix_variable, SciantixArray<SciantixVariable> &history_variable, SciantixArray<InputVariable> &scaling_factors)
{
    System system_;

    system_.setName("Xe in UO2HBS");
    system_.setGas(gas["Xe"]);
    system_.setMatrix(matrices["UO2HBS"]);
    system_.setRestructuredMatrix(1);
    system_.setYield(0.24);
    system_.setRadiusInLattice(0.21e-9);
    system_.setVolumeInLattice(matrices["UO2HBS"].getSchottkyVolume());
    system_.setHenryConstant(0.0);
    system_.setProductionRate(5, history_variable, input_variable, sciantix_variable, scaling_factors);
    system_.setFissionGasDiffusivity(5, sciantix_variable, history_variable, scaling_factors);
    system_.setBubbleDiffusivity(0, sciantix_variable, history_variable, matrices);
    system_.setResolutionRate(99, sciantix_variable, history_variable, scaling_factors, matrices);
    system_.setTrappingRate(99, sciantix_variable, scaling_factors);
    system_.setNucleationRate(99, history_variable, scaling_factors);

    return system_;
}

System Kr_in_UO2(SciantixArray<Matrix> &matrices, SciantixArray<Gas> &gas, SciantixArray<InputVariable> &input_variable,
    SciantixArray<SciantixVariable> &sciantix_variable, SciantixArray<SciantixVariable> &history_variable, SciantixArray<InputVariable> &scaling_factors)
{
    System system_;

    system_.setName("Kr in UO2");
    system_.setGas(gas["Kr"]);
    system_.setMatrix(matrices["UO2"]);
    system_.setRestructuredMatrix(0);
    system_.setYield(0.03);
    system_.setRadiusInLattice(0.21e-9);
    system_.setVolumeInLattice(matrices["UO2"].getSchottkyVolume());
    system_.setHenryConstant(0.0);
    system_.setProductionRate(1, history_variable, input_variable, sciantix_variable, scaling_factors);
    system_.setFissionGasDiffusivity(int(input_variable["iFissionGasDiffusivity"].getValue()), sciantix_variable, history_variable, scaling_factors);
    system_.setBubbleDiffusivity(int(input_variable["iBubbleDiffusivity"].getValue()), sciantix_variable, history_variable, matrices);
    system_.setResolutionRate(int(input_variable["iResolutionRate"].getValue()), sciantix_variable, history_variable, scaling_factors, matrices);
    system_.setTrappingRate(int(input_variable["iTrappingRate"].getValue()), sciantix_variable, scaling_factors);
    system_.setNucleationRate(int(input_variable["iNucleationRate"].getValue()), history_variable, scaling_factors);

    return system_;
}

System He_in_UO2(SciantixArray<Matrix> &matrices, SciantixArray<Gas> &gas, SciantixArray<InputVariable> &input_variable,
    SciantixArray<SciantixVariable> &sciantix_variable, SciantixArray<SciantixVariable> &history_variable, SciantixArray<InputVariable> &scaling_factors)
{
    System system_;

    system_.setName("He in UO2");
    system_.setGas(gas["He"]);
    system_.setMatrix(matrices["UO2"]);
    system_.setHenryConstant(4.1e+18 * exp(-7543.5 / history_variable["Temperature"].getFinalValue())); /// The Henry's constant for helium in UO<sub>2</sub>-single crystal samples is set from best estimate correlation after @ref *L. Cognini et al. Nuclear Engineering and Design 340 (2018) 240–244*. This correlation is valid in the temperature range 1073-1773 K.
    system_.setRestructuredMatrix(0);
    system_.setYield(0.0022); // from ternary fissions
    system_.setRadiusInLattice(4.73e-11);
    system_.setVolumeInLattice(matrices["UO2"].getOctahedralInterstitialSite());
    system_.setHeliumDiffusivity(int(input_variable["iHeDiffusivity"].getValue()), history_variable);
    system_.setResolutionRate(int(input_variable["iResolutionRate"].getValue()), sciantix_variable, history_variable, scaling_factors, matrices);
    system_.setTrappingRate(int(input_variable["iTrappingRate"].getValue()), sciantix_variable, scaling_factors);
    system_.setNucleationRate(int(input_variable["iNucleationRate"].getValue()), history_variable, scaling_factors);
    system_.setProductionRate(int(input_variable["iHeliumProductionRate"].getValue()), history_variable, input_variable, sciantix_variable, scaling_factors);
    system_.setBubbleDiffusivity(int(input_variable["iBubbleDiffusivity"].getValue()), sciantix_variable, history_variable, matrices);

    return system_;
}

System Xe133_in_UO2(SciantixArray<Matrix> &matrices, SciantixArray<Gas> &gas, SciantixArray<InputVariable> &input_variable,
    SciantixArray<SciantixVariable> &sciantix_variable, SciantixArray<SciantixVariable> &history_variable, SciantixArray<InputVariable> &scaling_factors)
{
    System system_;

    system_.setName("Xe133 in UO2");
    system_.setGas(gas["Xe133"]);
    system_.setMatrix(matrices["UO2"]);
    system_.setRestructuredMatrix(0);
    system_.setYield(0.066534); // from JEFF-3.3 library
    system_.setRadiusInLattice(0.21e-9); // (m), number from experimental results, assumed equal for Xe and Kr
    system_.setVolumeInLattice(matrices["UO2"].getSchottkyVolume());
    system_.setHenryConstant(0.0);
    system_.setProductionRate(1, history_variable, input_variable, sciantix_variable, scaling_factors);
    system_.setFissionGasDiffusivity(int(input_variable["iFissionGasDiffusivity"].getValue()), sciantix_variable, history_variable, scaling_factors);
    system_.setBubbleDiffusivity(int(input_variable["iBubbleDiffusivity"].getValue()), sciantix_variable, history_variable, matrices);
    system_.setResolutionRate(int(input_variable["iResolutionRate"].getValue()), sciantix_variable, history_variable, scaling_factors, matrices);
    system_.setTrappingRate(int(input_variable["iTrappingRate"].getValue()), sciantix_variable, scaling_factors);
    system_.setNucleationRate(int(input_variable["iNucleationRate"].getValue()), history_variable, scaling_factors);

    return system_;
}

System Kr85m_in_UO2(SciantixArray<Matrix> &matrices, SciantixArray<Gas> &gas, SciantixArray<InputVariable> &input_variable,
    SciantixArray<SciantixVariable> &sciantix_variable, SciantixArray<SciantixVariable> &history_variable, SciantixArray<InputVariable> &scaling_factors)
{
    System system_;

    system_.setName("Kr85m in UO2");
    system_.setGas(gas["Kr85m"]);
    system_.setMatrix(matrices["UO2"]);
    system_.setRestructuredMatrix(0);
    system_.setYield(0.013027);
    system_.setRadiusInLattice(0.21e-9);
    system_.setVolumeInLattice(matrices["UO2"].getSchottkyVolume());
    system_.setHenryConstant(0.0);
    system_.setProductionRate(1, history_variable, input_variable, sciantix_variable, scaling_factors);
    system_.setFissionGasDiffusivity(int(input_variable["iFissionGasDiffusivity"].getValue()), sciantix_variable, history_variable, scaling_factors);
    system_.setBubbleDiffusivity(int(input_variable["iBubbleDiffusivity"].getValue()), sciantix_variable, history_variable, matrices);
    system_.setResolutionRate(int(input_variable["iResolutionRate"].getValue()), sciantix_variable, history_variable, scaling_factors, matrices);
    system_.setTrappingRate(int(input_variable["iTrappingRate"].getValue()), sciantix_variable, scaling_factors);
    system_.setNucleationRate(int(input_variable["iNucleationRate"].getValue()), history_variable, scaling_factors);

    return system_;
}