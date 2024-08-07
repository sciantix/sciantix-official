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

#include "SetSystem.h"
#include "Simulation.h"

/// SetSystem

void Simulation::setSystem()
{
    switch ((int)input_variable["iFuelMatrix"].getValue())
    {
        case 0:
            sciantix_system.push(Xe_in_UO2(matrices, gas, input_variable, sciantix_variable, history_variable, scaling_factors));
            sciantix_system.push(Kr_in_UO2(matrices, gas, input_variable, sciantix_variable, history_variable, scaling_factors));
            sciantix_system.push(He_in_UO2(matrices, gas, input_variable, sciantix_variable, history_variable, scaling_factors));
            sciantix_system.push(Xe133_in_UO2(matrices, gas, input_variable, sciantix_variable, history_variable, scaling_factors));
            sciantix_system.push(Kr85m_in_UO2(matrices, gas, input_variable, sciantix_variable, history_variable, scaling_factors));
            break;

        case 1:
            sciantix_system.push(Xe_in_UO2(matrices, gas, input_variable, sciantix_variable, history_variable, scaling_factors));
            sciantix_system.push(Xe_in_UO2HBS(matrices, gas, input_variable, sciantix_variable, history_variable, scaling_factors));
            break;

        default:
            break;
    }
}




System Xe_in_UO2(SciantixArray<Matrix> matrices, SciantixArray<Gas> gas, SciantixArray<InputVariable> input_variable,
	SciantixArray<SciantixVariable> sciantix_variable, SciantixArray<SciantixVariable> history_variable, SciantixArray<InputVariable> scaling_factors)
{
    System result;

    result.setName("Xe in UO2");
	result.setGas(gas["Xe"]);
	result.setMatrix(matrices["UO2"]);
	result.setRestructuredMatrix(0);
	result.setYield(0.24);
	result.setRadiusInLattice(0.21e-9); // (m), from experimental data, assumed equal for Xe and Kr
	result.setVolumeInLattice(matrices["UO2"].getSchottkyVolume());
	result.setHenryConstant(0.0);
	result.setProductionRate(1, history_variable, input_variable, sciantix_variable, scaling_factors);
	result.setFissionGasDiffusivity(int(input_variable["iFGDiffusionCoefficient"].getValue()), sciantix_variable, history_variable, scaling_factors);
	result.setBubbleDiffusivity(int(input_variable["iBubbleDiffusivity"].getValue()), sciantix_variable, history_variable, matrices);
	result.setResolutionRate(int(input_variable["iResolutionRate"].getValue()), sciantix_variable, history_variable, scaling_factors, matrices);
	result.setTrappingRate(int(input_variable["iTrappingRate"].getValue()), sciantix_variable, scaling_factors);
	result.setNucleationRate(int(input_variable["iNucleationRate"].getValue()), history_variable, scaling_factors);

    return result;
}

System Xe_in_UO2HBS(SciantixArray<Matrix> matrices, SciantixArray<Gas> gas, SciantixArray<InputVariable> input_variable,
	SciantixArray<SciantixVariable> sciantix_variable, SciantixArray<SciantixVariable> history_variable, SciantixArray<InputVariable> scaling_factors)
{
    System result;

    result.setName("Xe in UO2HBS");
	result.setGas(gas["Xe"]);
	result.setMatrix(matrices["UO2HBS"]);
	result.setRestructuredMatrix(1);
	result.setYield(0.24);
	result.setRadiusInLattice(0.21e-9);
	result.setVolumeInLattice(matrices["UO2HBS"].getSchottkyVolume());
	result.setHenryConstant(0.0);
	result.setProductionRate(5, history_variable, input_variable, sciantix_variable, scaling_factors);
	result.setFissionGasDiffusivity(5, sciantix_variable, history_variable, scaling_factors);
	result.setBubbleDiffusivity(0, sciantix_variable, history_variable, matrices);
	result.setResolutionRate(99, sciantix_variable, history_variable, scaling_factors, matrices);
	result.setTrappingRate(99, sciantix_variable, scaling_factors);
	result.setNucleationRate(99, history_variable, scaling_factors);

    return result;
}

System Kr_in_UO2(SciantixArray<Matrix> matrices, SciantixArray<Gas> gas, SciantixArray<InputVariable> input_variable,
	SciantixArray<SciantixVariable> sciantix_variable, SciantixArray<SciantixVariable> history_variable, SciantixArray<InputVariable> scaling_factors)
{
    System result;

    result.setName("Kr in UO2");
	result.setGas(gas["Kr"]);
	result.setMatrix(matrices["UO2"]);
	result.setRestructuredMatrix(0);
	result.setYield(0.03);
	result.setRadiusInLattice(0.21e-9);
	result.setVolumeInLattice(matrices["UO2"].getSchottkyVolume());
	result.setHenryConstant(0.0);
	result.setProductionRate(1, history_variable, input_variable, sciantix_variable, scaling_factors);
	result.setFissionGasDiffusivity(int(input_variable["iFGDiffusionCoefficient"].getValue()), sciantix_variable, history_variable, scaling_factors);
	result.setBubbleDiffusivity(int(input_variable["iBubbleDiffusivity"].getValue()), sciantix_variable, history_variable, matrices);
	result.setResolutionRate(int(input_variable["iResolutionRate"].getValue()), sciantix_variable, history_variable, scaling_factors, matrices);
	result.setTrappingRate(int(input_variable["iTrappingRate"].getValue()), sciantix_variable, scaling_factors);
	result.setNucleationRate(int(input_variable["iNucleationRate"].getValue()), history_variable, scaling_factors);

    return result;
}

System He_in_UO2(SciantixArray<Matrix> matrices, SciantixArray<Gas> gas, SciantixArray<InputVariable> input_variable,
	SciantixArray<SciantixVariable> sciantix_variable, SciantixArray<SciantixVariable> history_variable, SciantixArray<InputVariable> scaling_factors)
{
    System result;

    result.setName("He in UO2");
	result.setGas(gas["He"]);
	result.setMatrix(matrices["UO2"]);
	result.setHenryConstant(4.1e+18 * exp(-7543.5 / history_variable["Temperature"].getFinalValue())); /// The Henry's constant for helium in UO<sub>2</sub>-single crystal samples is set from best estimate correlation after @ref *L. Cognini et al. Nuclear Engineering and Design 340 (2018) 240–244*. This correlation is valid in the temperature range 1073-1773 K.
	result.setRestructuredMatrix(0);
	result.setYield(0.0022); // from ternary fissions
	result.setRadiusInLattice(4.73e-11);
	result.setVolumeInLattice(matrices["UO2"].getOIS());
	result.setHeliumDiffusivity(int(input_variable["iHeDiffusivity"].getValue()), history_variable);
	result.setResolutionRate(int(input_variable["iResolutionRate"].getValue()), sciantix_variable, history_variable, scaling_factors, matrices);
	result.setTrappingRate(int(input_variable["iTrappingRate"].getValue()), sciantix_variable, scaling_factors);
	result.setNucleationRate(int(input_variable["iNucleationRate"].getValue()), history_variable, scaling_factors);
	result.setProductionRate(int(input_variable["iHeliumProductionRate"].getValue()), history_variable, input_variable, sciantix_variable, scaling_factors);
	result.setBubbleDiffusivity(int(input_variable["iBubbleDiffusivity"].getValue()), sciantix_variable, history_variable, matrices);

    return result;
}

System Xe133_in_UO2(SciantixArray<Matrix> matrices, SciantixArray<Gas> gas, SciantixArray<InputVariable> input_variable,
	SciantixArray<SciantixVariable> sciantix_variable, SciantixArray<SciantixVariable> history_variable, SciantixArray<InputVariable> scaling_factors)
{
    System result;

    result.setName("Xe133 in UO2");
	result.setGas(gas["Xe133"]);
	result.setMatrix(matrices["UO2"]);
	result.setRestructuredMatrix(0);
	result.setYield(0.066534); // from JEFF-3.3 library
	result.setRadiusInLattice(0.21e-9); // (m), number from experimental results, assumed equal for Xe and Kr
	result.setVolumeInLattice(matrices["UO2"].getSchottkyVolume());
	result.setHenryConstant(0.0);
	result.setProductionRate(1, history_variable, input_variable, sciantix_variable, scaling_factors);
	result.setFissionGasDiffusivity(int(input_variable["iFGDiffusionCoefficient"].getValue()), sciantix_variable, history_variable, scaling_factors);
	result.setBubbleDiffusivity(int(input_variable["iBubbleDiffusivity"].getValue()), sciantix_variable, history_variable, matrices);
	result.setResolutionRate(int(input_variable["iResolutionRate"].getValue()), sciantix_variable, history_variable, scaling_factors, matrices);
	result.setTrappingRate(int(input_variable["iTrappingRate"].getValue()), sciantix_variable, scaling_factors);
	result.setNucleationRate(int(input_variable["iNucleationRate"].getValue()), history_variable, scaling_factors);

    return result;
}

System Kr85m_in_UO2(SciantixArray<Matrix> matrices, SciantixArray<Gas> gas, SciantixArray<InputVariable> input_variable,
	SciantixArray<SciantixVariable> sciantix_variable, SciantixArray<SciantixVariable> history_variable, SciantixArray<InputVariable> scaling_factors)
{
    System result;

    result.setName("Kr85m in UO2");
	result.setGas(gas["Kr85m"]);
	result.setMatrix(matrices["UO2"]);
	result.setRestructuredMatrix(0);
	result.setYield(0.013027);
	result.setRadiusInLattice(0.21e-9);
	result.setVolumeInLattice(matrices["UO2"].getSchottkyVolume());
	result.setHenryConstant(0.0);
	result.setProductionRate(1, history_variable, input_variable, sciantix_variable, scaling_factors);
	result.setFissionGasDiffusivity(int(input_variable["iFGDiffusionCoefficient"].getValue()), sciantix_variable, history_variable, scaling_factors);
	result.setBubbleDiffusivity(int(input_variable["iBubbleDiffusivity"].getValue()), sciantix_variable, history_variable, matrices);
	result.setResolutionRate(int(input_variable["iResolutionRate"].getValue()), sciantix_variable, history_variable, scaling_factors, matrices);
	result.setTrappingRate(int(input_variable["iTrappingRate"].getValue()), sciantix_variable, scaling_factors);
	result.setNucleationRate(int(input_variable["iNucleationRate"].getValue()), history_variable, scaling_factors);

    return result;
}