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
            sciantix_system.push(Kr_in_UO2(matrices, gas, input_variable, sciantix_variable, history_variable, scaling_factors));
            sciantix_system.push(He_in_UO2(matrices, gas, input_variable, sciantix_variable, history_variable, scaling_factors));
            sciantix_system.push(Xe133_in_UO2(matrices, gas, input_variable, sciantix_variable, history_variable, scaling_factors));
            sciantix_system.push(Kr85m_in_UO2(matrices, gas, input_variable, sciantix_variable, history_variable, scaling_factors));
            sciantix_system.push(Cs_in_UO2(matrices, gas, input_variable, sciantix_variable, history_variable, scaling_factors));            
            sciantix_system.push(I_in_UO2(matrices, gas, input_variable, sciantix_variable, history_variable, scaling_factors));
            sciantix_system.push(Te_in_UO2(matrices, gas, input_variable, sciantix_variable, history_variable, scaling_factors));
            break;
            break;

        case 1:
            sciantix_system.push(Xe_in_UO2(matrices, gas, input_variable, sciantix_variable, history_variable, scaling_factors));
            sciantix_system.push(Xe_in_UO2HBS(matrices, gas, input_variable, sciantix_variable, history_variable, scaling_factors));
            break;

        case 2:
            sciantix_system.push(Xe_in_MOX(matrices, gas, input_variable, sciantix_variable, history_variable, scaling_factors));
            sciantix_system.push(Kr_in_MOX(matrices, gas, input_variable, sciantix_variable, history_variable, scaling_factors));
            sciantix_system.push(He_in_MOX(matrices, gas, input_variable, sciantix_variable, history_variable, scaling_factors));
            sciantix_system.push(Xe133_in_MOX(matrices, gas, input_variable, sciantix_variable, history_variable, scaling_factors));
            sciantix_system.push(Kr85m_in_MOX(matrices, gas, input_variable, sciantix_variable, history_variable, scaling_factors));
            sciantix_system.push(Cs_in_MOX(matrices, gas, input_variable, sciantix_variable, history_variable, scaling_factors));            
            sciantix_system.push(I_in_MOX(matrices, gas, input_variable, sciantix_variable, history_variable, scaling_factors));
            sciantix_system.push(Te_in_MOX(matrices, gas, input_variable, sciantix_variable, history_variable, scaling_factors));
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
    system_.setYield(sciantix_variable);
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
    system_.setYield(sciantix_variable);
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
    system_.setYield(sciantix_variable);
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
    system_.setYield(sciantix_variable);
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
    system_.setYield(sciantix_variable);
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
    system_.setYield(sciantix_variable);
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

System Cs_in_UO2(SciantixArray<Matrix> &matrices, SciantixArray<Gas> &gas, SciantixArray<InputVariable> &input_variable,
    SciantixArray<SciantixVariable> &sciantix_variable, SciantixArray<SciantixVariable> &history_variable, SciantixArray<InputVariable> &scaling_factors)
{
    System system_;

    system_.setName("Cs in UO2");
    system_.setGas(gas["Cs"]);
    system_.setMatrix(matrices["UO2"]);
    system_.setRestructuredMatrix(0);
    system_.setYield(sciantix_variable);
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

System I_in_UO2(SciantixArray<Matrix> &matrices, SciantixArray<Gas> &gas, SciantixArray<InputVariable> &input_variable,
    SciantixArray<SciantixVariable> &sciantix_variable, SciantixArray<SciantixVariable> &history_variable, SciantixArray<InputVariable> &scaling_factors)
{
    System system_;

    system_.setName("I in UO2");
    system_.setGas(gas["I"]);
    system_.setMatrix(matrices["UO2"]);
    system_.setRestructuredMatrix(0);
    system_.setYield(sciantix_variable);
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

System Te_in_UO2(SciantixArray<Matrix> &matrices, SciantixArray<Gas> &gas, SciantixArray<InputVariable> &input_variable,
    SciantixArray<SciantixVariable> &sciantix_variable, SciantixArray<SciantixVariable> &history_variable, SciantixArray<InputVariable> &scaling_factors)
{
    System system_;

    system_.setName("Te in UO2");
    system_.setGas(gas["Te"]);
    system_.setMatrix(matrices["UO2"]);
    system_.setRestructuredMatrix(0);
    system_.setYield(sciantix_variable);
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

// MOX

System Xe_in_MOX(SciantixArray<Matrix> &matrices, SciantixArray<Gas> &gas, SciantixArray<InputVariable> &input_variable,
    SciantixArray<SciantixVariable> &sciantix_variable, SciantixArray<SciantixVariable> &history_variable, SciantixArray<InputVariable> &scaling_factors)
{
    System system_;

    system_.setName("Xe in MOX");
    system_.setGas(gas["Xe"]);
    system_.setMatrix(matrices["MOX"]);
    system_.setRestructuredMatrix(0);
    system_.setYield(sciantix_variable); 
    system_.setRadiusInLattice(0.21e-9); // (m), from experimental data, assumed equal for Xe and Kr, equal to UO2
    system_.setVolumeInLattice(matrices["MOX"].getSchottkyVolume());
    system_.setHenryConstant(0.0); // Rest, 1992; Walker, 1977 for Xe and Kr typical value is 10^-7
    system_.setProductionRate(1, history_variable, input_variable, sciantix_variable, scaling_factors); // Production rate as in UO₂ (Turnbull, 1988)
    system_.setFissionGasDiffusivity(int(input_variable["iFissionGasDiffusivity"].getValue()), sciantix_variable, history_variable, scaling_factors); // Lanning (2005); OECD/NEA (2019) – higher diffusivity in MOX due to porosity and Pu content 1.75 * for MOX?
    system_.setBubbleDiffusivity(int(input_variable["iBubbleDiffusivity"].getValue()), sciantix_variable, history_variable, matrices);
    system_.setResolutionRate(int(input_variable["iResolutionRate"].getValue()), sciantix_variable, history_variable, scaling_factors, matrices);
    system_.setTrappingRate(int(input_variable["iTrappingRate"].getValue()), sciantix_variable, scaling_factors);
    system_.setNucleationRate(int(input_variable["iNucleationRate"].getValue()), history_variable, scaling_factors);

    return system_;
}

System Kr_in_MOX(SciantixArray<Matrix> &matrices, SciantixArray<Gas> &gas, SciantixArray<InputVariable> &input_variable,
    SciantixArray<SciantixVariable> &sciantix_variable, SciantixArray<SciantixVariable> &history_variable, SciantixArray<InputVariable> &scaling_factors)
{
    System system_;

    system_.setName("Kr in MOX");
    system_.setGas(gas["Kr"]);
    system_.setMatrix(matrices["MOX"]);
    system_.setRestructuredMatrix(0);
    system_.setYield(sciantix_variable);
    system_.setRadiusInLattice(0.21e-9); // (m), from experimental data, assumed equal for Xe and Kr, equal to UO2
    system_.setVolumeInLattice(matrices["MOX"].getSchottkyVolume());
    system_.setHenryConstant(0.0); // Rest, 1992; Walker, 1977 for Xe and Kr typical value is 10^-7
    system_.setProductionRate(1, history_variable, input_variable, sciantix_variable, scaling_factors); // MOX: using same production model as UO2 (no dedicated MOX correlation)
    system_.setFissionGasDiffusivity(int(input_variable["iFissionGasDiffusivity"].getValue()), sciantix_variable, history_variable, scaling_factors); // Lanning (2005); OECD/NEA (2019) – higher diffusivity in MOX due to porosity and Pu content 1.75 * for MOX?
    system_.setBubbleDiffusivity(int(input_variable["iBubbleDiffusivity"].getValue()), sciantix_variable, history_variable, matrices);
    system_.setResolutionRate(int(input_variable["iResolutionRate"].getValue()), sciantix_variable, history_variable, scaling_factors, matrices);
    system_.setTrappingRate(int(input_variable["iTrappingRate"].getValue()), sciantix_variable, scaling_factors);
    system_.setNucleationRate(int(input_variable["iNucleationRate"].getValue()), history_variable, scaling_factors);

    return system_;
}

System He_in_MOX(SciantixArray<Matrix> &matrices, SciantixArray<Gas> &gas, SciantixArray<InputVariable> &input_variable,
    SciantixArray<SciantixVariable> &sciantix_variable, SciantixArray<SciantixVariable> &history_variable, SciantixArray<InputVariable> &scaling_factors)
{
    System system_;

    system_.setName("He in MOX");
    system_.setGas(gas["He"]);
    system_.setMatrix(matrices["MOX"]);
    system_.setHenryConstant(4.1e+18 * exp(-7543.5 / history_variable["Temperature"].getFinalValue())); // from Matzke, JNM 65 (1977) 89-106, correlation used for UO2 and MOX
    system_.setRestructuredMatrix(0);
    system_.setYield(sciantix_variable);
    system_.setRadiusInLattice(4.73e-11); // (m), value of UO₂, valid also for MOX
    system_.setVolumeInLattice(matrices["MOX"].getOctahedralInterstitialSite());
    system_.setHeliumDiffusivity(int(input_variable["iHeDiffusivity"].getValue()), history_variable);
    system_.setResolutionRate(int(input_variable["iResolutionRate"].getValue()), sciantix_variable, history_variable, scaling_factors, matrices);
    system_.setTrappingRate(int(input_variable["iTrappingRate"].getValue()), sciantix_variable, scaling_factors);
    system_.setNucleationRate(int(input_variable["iNucleationRate"].getValue()), history_variable, scaling_factors);
    system_.setProductionRate(2, history_variable, input_variable, sciantix_variable, scaling_factors); // A. Cechet et al., Nuclear Engineering and Technology (2021), case 2 in system.c
    system_.setBubbleDiffusivity(int(input_variable["iBubbleDiffusivity"].getValue()), sciantix_variable, history_variable, matrices);

    return system_;
}

System Xe133_in_MOX(SciantixArray<Matrix> &matrices, SciantixArray<Gas> &gas, SciantixArray<InputVariable> &input_variable,
    SciantixArray<SciantixVariable> &sciantix_variable, SciantixArray<SciantixVariable> &history_variable, SciantixArray<InputVariable> &scaling_factors)
{
    System system_;

    system_.setName("Xe133 in MOX");
    system_.setGas(gas["Xe133"]);
    system_.setMatrix(matrices["MOX"]);
    system_.setRestructuredMatrix(0);
    system_.setYield(sciantix_variable);
    system_.setRadiusInLattice(0.21e-9); // (m), from experimental data, assumed equal for Xe and Kr, equal to UO2
    system_.setVolumeInLattice(matrices["MOX"].getSchottkyVolume());
    system_.setHenryConstant(0.0); // Rest, 1992; Walker, 1977 for Xe and Kr typical value is 10^-7
    system_.setProductionRate(1, history_variable, input_variable, sciantix_variable, scaling_factors); // MOX: using same production model as UO2 (no dedicated MOX correlation)
    system_.setFissionGasDiffusivity(int(input_variable["iFissionGasDiffusivity"].getValue()), sciantix_variable, history_variable, scaling_factors); // Lanning (2005); OECD/NEA (2019) – higher diffusivity in MOX due to porosity and Pu content
    system_.setBubbleDiffusivity(int(input_variable["iBubbleDiffusivity"].getValue()), sciantix_variable, history_variable, matrices);
    system_.setResolutionRate(int(input_variable["iResolutionRate"].getValue()), sciantix_variable, history_variable, scaling_factors, matrices);
    system_.setTrappingRate(int(input_variable["iTrappingRate"].getValue()), sciantix_variable, scaling_factors);
    system_.setNucleationRate(int(input_variable["iNucleationRate"].getValue()), history_variable, scaling_factors);

    return system_;
}

System Kr85m_in_MOX(SciantixArray<Matrix> &matrices, SciantixArray<Gas> &gas, SciantixArray<InputVariable> &input_variable,
    SciantixArray<SciantixVariable> &sciantix_variable, SciantixArray<SciantixVariable> &history_variable, SciantixArray<InputVariable> &scaling_factors)
{
    System system_;

    system_.setName("Kr85m in MOX");
    system_.setGas(gas["Kr85m"]);
    system_.setMatrix(matrices["MOX"]);
    system_.setRestructuredMatrix(0);
    system_.setYield(sciantix_variable);
    system_.setRadiusInLattice(0.21e-9); // (m), from experimental data, assumed equal for Xe and Kr, equal to UO2
    system_.setVolumeInLattice(matrices["MOX"].getSchottkyVolume());
    system_.setHenryConstant(0.0); // Rest, 1992; Walker, 1977 for Xe and Kr typical value is 10^-7
    system_.setProductionRate(1, history_variable, input_variable, sciantix_variable, scaling_factors); // MOX: using same production model as UO2 (no dedicated MOX correlation)
    system_.setFissionGasDiffusivity(int(input_variable["iFissionGasDiffusivity"].getValue()), sciantix_variable, history_variable, scaling_factors); // Lanning (2005); OECD/NEA (2019) – higher diffusivity in MOX due to porosity and Pu content 1.75 * for MOX?
    system_.setBubbleDiffusivity(int(input_variable["iBubbleDiffusivity"].getValue()), sciantix_variable, history_variable, matrices);
    system_.setResolutionRate(int(input_variable["iResolutionRate"].getValue()), sciantix_variable, history_variable, scaling_factors, matrices);
    system_.setTrappingRate(int(input_variable["iTrappingRate"].getValue()), sciantix_variable, scaling_factors);
    system_.setNucleationRate(int(input_variable["iNucleationRate"].getValue()), history_variable, scaling_factors);

    return system_;
}

System Cs_in_MOX(SciantixArray<Matrix> &matrices, SciantixArray<Gas> &gas, SciantixArray<InputVariable> &input_variable,
    SciantixArray<SciantixVariable> &sciantix_variable, SciantixArray<SciantixVariable> &history_variable, SciantixArray<InputVariable> &scaling_factors)
{
    System system_;

    system_.setName("Cs in MOX");
    system_.setGas(gas["Cs"]);
    system_.setMatrix(matrices["MOX"]);
    system_.setRestructuredMatrix(0);
    system_.setYield(sciantix_variable);
    system_.setRadiusInLattice(0.21e-9);
    system_.setVolumeInLattice(matrices["MOX"].getSchottkyVolume());
    system_.setHenryConstant(0.0);
    system_.setProductionRate(1, history_variable, input_variable, sciantix_variable, scaling_factors); // MOX: using same production model as UO2 (no dedicated MOX correlation)
    system_.setFissionGasDiffusivity(int(input_variable["iFissionGasDiffusivity"].getValue()), sciantix_variable, history_variable, scaling_factors); // Lanning (2005); OECD/NEA (2019) – higher diffusivity in MOX due to porosity and Pu content 1.75 * for MOX?
    system_.setBubbleDiffusivity(int(input_variable["iBubbleDiffusivity"].getValue()), sciantix_variable, history_variable, matrices);
    system_.setResolutionRate(int(input_variable["iResolutionRate"].getValue()), sciantix_variable, history_variable, scaling_factors, matrices);
    system_.setTrappingRate(int(input_variable["iTrappingRate"].getValue()), sciantix_variable, scaling_factors);
    system_.setNucleationRate(int(input_variable["iNucleationRate"].getValue()), history_variable, scaling_factors);

    return system_;
}

System I_in_MOX(SciantixArray<Matrix> &matrices, SciantixArray<Gas> &gas, SciantixArray<InputVariable> &input_variable,
    SciantixArray<SciantixVariable> &sciantix_variable, SciantixArray<SciantixVariable> &history_variable, SciantixArray<InputVariable> &scaling_factors)
{
    System system_;

    system_.setName("I in MOX");
    system_.setGas(gas["I"]);
    system_.setMatrix(matrices["MOX"]);
    system_.setRestructuredMatrix(0);
    system_.setYield(sciantix_variable);
    system_.setRadiusInLattice(0.21e-9);
    system_.setVolumeInLattice(matrices["MOX"].getSchottkyVolume());
    system_.setHenryConstant(0.0);
    system_.setProductionRate(1, history_variable, input_variable, sciantix_variable, scaling_factors); // MOX: using same production model as UO2 (no dedicated MOX correlation)
    system_.setFissionGasDiffusivity(int(input_variable["iFissionGasDiffusivity"].getValue()), sciantix_variable, history_variable, scaling_factors); // Lanning (2005); OECD/NEA (2019) – higher diffusivity in MOX due to porosity and Pu content 1.75 * for MOX?
    system_.setBubbleDiffusivity(int(input_variable["iBubbleDiffusivity"].getValue()), sciantix_variable, history_variable, matrices);
    system_.setResolutionRate(int(input_variable["iResolutionRate"].getValue()), sciantix_variable, history_variable, scaling_factors, matrices);
    system_.setTrappingRate(int(input_variable["iTrappingRate"].getValue()), sciantix_variable, scaling_factors);
    system_.setNucleationRate(int(input_variable["iNucleationRate"].getValue()), history_variable, scaling_factors);

    return system_;
}

System Te_in_MOX(SciantixArray<Matrix> &matrices, SciantixArray<Gas> &gas, SciantixArray<InputVariable> &input_variable,
    SciantixArray<SciantixVariable> &sciantix_variable, SciantixArray<SciantixVariable> &history_variable, SciantixArray<InputVariable> &scaling_factors)
{
    System system_;

    system_.setName("Te in MOX");
    system_.setGas(gas["Te"]);
    system_.setMatrix(matrices["MOX"]);
    system_.setRestructuredMatrix(0);
    system_.setYield(sciantix_variable);
    system_.setRadiusInLattice(0.21e-9);
    system_.setVolumeInLattice(matrices["MOX"].getSchottkyVolume());
    system_.setHenryConstant(0.0);
    system_.setProductionRate(1, history_variable, input_variable, sciantix_variable, scaling_factors); // MOX: using same production model as UO2 (no dedicated MOX correlation)
    system_.setFissionGasDiffusivity(int(input_variable["iFissionGasDiffusivity"].getValue()), sciantix_variable, history_variable, scaling_factors); // Lanning (2005); OECD/NEA (2019) – higher diffusivity in MOX due to porosity and Pu content 1.75 * for MOX?
    system_.setBubbleDiffusivity(int(input_variable["iBubbleDiffusivity"].getValue()), sciantix_variable, history_variable, matrices);
    system_.setResolutionRate(int(input_variable["iResolutionRate"].getValue()), sciantix_variable, history_variable, scaling_factors, matrices);
    system_.setTrappingRate(int(input_variable["iTrappingRate"].getValue()), sciantix_variable, scaling_factors);
    system_.setNucleationRate(int(input_variable["iNucleationRate"].getValue()), history_variable, scaling_factors);

    return system_;
}