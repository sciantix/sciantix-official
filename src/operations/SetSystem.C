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
//  Version: 2.5                                                                    //
//  Year: 2026                                                                      //
//  Authors: D. Pizzocri, G. Zullo, E. Cappellari.                                  //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "SetSystem.h"
#include "Simulation.h"

#include <cmath>
#include <map>     // [FMP, Nov. 2026]
#include <string>  // [FMP, Nov. 2026]

void Simulation::setSystem()
{
    switch ((int)input_variable["iFuelMatrix"].getValue())
    {
        case 0:
            sciantix_system.push(
                Xe_in_UO2(matrices, gas_fp, input_variable, sciantix_variable, history_variable, scaling_factors));
            sciantix_system.push(
                Kr_in_UO2(matrices, gas_fp, input_variable, sciantix_variable, history_variable, scaling_factors));
            sciantix_system.push(
                He_in_UO2(matrices, gas_fp, input_variable, sciantix_variable, history_variable, scaling_factors));
            sciantix_system.push(
                Xe133_in_UO2(matrices, gas_fp, input_variable, sciantix_variable, history_variable, scaling_factors));
#if !defined(COUPLING_TU)
            // Kr85m diffusion modes (blocks 12-14) are not tracked by TRANSURANUS.
            sciantix_system.push(
                Kr85m_in_UO2(matrices, gas_fp, input_variable, sciantix_variable, history_variable, scaling_factors));
#endif
            // EC November 2026: Mo, Tc, Ru, Rh, Pd in UO2 (LWR yields) for the five metal particles (FMP) model
            if ((int)input_variable["iFMP"].getValue() == 1)
            {
                for (const auto& element : {"Mo", "Tc", "Ru", "Rh", "Pd"})
                    sciantix_system.push(MetallicFP_in_UO2(element,
                                                           matrices,
                                                           metallic_fp,
                                                           input_variable,
                                                           sciantix_variable,
                                                           history_variable,
                                                           scaling_factors));
            }
            break;

        case 1:
            sciantix_system.push(
                Xe_in_UO2(matrices, gas_fp, input_variable, sciantix_variable, history_variable, scaling_factors));
#if !defined(COUPLING_TU)
            // Xe-in-HBS diffusion modes (blocks 15-17) are not tracked by TRANSURANUS.
            sciantix_system.push(
                Xe_in_UO2HBS(matrices, gas_fp, input_variable, sciantix_variable, history_variable, scaling_factors));
#endif
            // [FMP, Nov. 2026]: Mo, Tc, Ru, Rh, Pd in UO2 (LWR yields) for the five metal particles (FMP) model
            if ((int)input_variable["iFMP"].getValue() == 1)
            {
                for (const auto& element : {"Mo", "Tc", "Ru", "Rh", "Pd"})
                    sciantix_system.push(MetallicFP_in_UO2(element,
                                                           matrices,
                                                           metallic_fp,
                                                           input_variable,
                                                           sciantix_variable,
                                                           history_variable,
                                                           scaling_factors));
            }
            break;

        case 2:
            sciantix_system.push(
                Xe_in_MOX(matrices, gas_fp, input_variable, sciantix_variable, history_variable, scaling_factors));
            sciantix_system.push(
                Kr_in_MOX(matrices, gas_fp, input_variable, sciantix_variable, history_variable, scaling_factors));
            sciantix_system.push(
                He_in_MOX(matrices, gas_fp, input_variable, sciantix_variable, history_variable, scaling_factors));
            sciantix_system.push(
                Xe133_in_MOX(matrices, gas_fp, input_variable, sciantix_variable, history_variable, scaling_factors));
#if !defined(COUPLING_TU)
            // Kr85m diffusion modes (blocks 12-14) are not tracked by TRANSURANUS.
            sciantix_system.push(
                Kr85m_in_MOX(matrices, gas_fp, input_variable, sciantix_variable, history_variable, scaling_factors));
#endif

            if ((int)input_variable["iThermochimica"].getValue() == 0)
            {
                // [FMP, Nov. 2026]: the FMP model needs the five metals also without thermochemistry
                if ((int)input_variable["iFMP"].getValue() == 1)
                {
                    sciantix_system.push(Mo_in_MOX(
                        matrices, metallic_fp, input_variable, sciantix_variable, history_variable, scaling_factors));
                    sciantix_system.push(Tc_in_MOX(
                        matrices, metallic_fp, input_variable, sciantix_variable, history_variable, scaling_factors));
                    sciantix_system.push(Rh_in_MOX(
                        matrices, metallic_fp, input_variable, sciantix_variable, history_variable, scaling_factors));
                    sciantix_system.push(Ru_in_MOX(
                        matrices, metallic_fp, input_variable, sciantix_variable, history_variable, scaling_factors));
                    sciantix_system.push(Pd_in_MOX(
                        matrices, metallic_fp, input_variable, sciantix_variable, history_variable, scaling_factors));
                }
                break;
            }

            sciantix_system.push(
                Cs_in_MOX(matrices, volatile_fp, input_variable, sciantix_variable, history_variable, scaling_factors));
            sciantix_system.push(
                Mo_in_MOX(matrices, metallic_fp, input_variable, sciantix_variable, history_variable, scaling_factors));
            sciantix_system.push(
                Ba_in_MOX(matrices, ceramic_fp, input_variable, sciantix_variable, history_variable, scaling_factors));
            sciantix_system.push(
                Tc_in_MOX(matrices, metallic_fp, input_variable, sciantix_variable, history_variable, scaling_factors));
            sciantix_system.push(
                Rh_in_MOX(matrices, metallic_fp, input_variable, sciantix_variable, history_variable, scaling_factors));
            sciantix_system.push(
                Ru_in_MOX(matrices, metallic_fp, input_variable, sciantix_variable, history_variable, scaling_factors));
            sciantix_system.push(
                Pd_in_MOX(matrices, metallic_fp, input_variable, sciantix_variable, history_variable, scaling_factors));

        default:
            break;
    }
}

System Xe_in_UO2(SciantixArray<Matrix>&           matrices,
                 SciantixArray<FissionProducts>&  gas_fp,
                 SciantixArray<InputVariable>&    input_variable,
                 SciantixArray<SciantixVariable>& sciantix_variable,
                 SciantixArray<SciantixVariable>& history_variable,
                 SciantixArray<InputVariable>&    scaling_factors)
{
    System system_;

    system_.setName("Xe in UO2");
    system_.setGas(gas_fp["Xe"]);
    system_.setMatrix(matrices["UO2"]);
    system_.setRestructuredMatrix(0);
    system_.setYield(0.24);
    system_.setRadiusInLattice(0.21e-9);  // (m), from experimental data, assumed equal for Xe and Kr
    system_.setVolumeInLattice(matrices["UO2"].getSchottkyVolume());
    system_.setHenryConstant(0.0);
    system_.setProductionRate(1, history_variable, input_variable, sciantix_variable, scaling_factors);
    system_.setFissionProductDiffusivity(int(input_variable["iFissionProductDiffusivity"].getValue()),
                                         sciantix_variable,
                                         history_variable,
                                         scaling_factors,
                                         matrices);
    system_.setBubbleDiffusivity(
        int(input_variable["iBubbleDiffusivity"].getValue()), sciantix_variable, history_variable, matrices);
    system_.setResolutionRate(int(input_variable["iResolutionRate"].getValue()),
                              sciantix_variable,
                              history_variable,
                              scaling_factors,
                              matrices);
    system_.setTrappingRate(int(input_variable["iTrappingRate"].getValue()), sciantix_variable, scaling_factors);
    system_.setNucleationRate(int(input_variable["iNucleationRate"].getValue()), history_variable, scaling_factors);

    return system_;
}

System Xe_in_UO2HBS(SciantixArray<Matrix>&           matrices,
                    SciantixArray<FissionProducts>&  gas_fp,
                    SciantixArray<InputVariable>&    input_variable,
                    SciantixArray<SciantixVariable>& sciantix_variable,
                    SciantixArray<SciantixVariable>& history_variable,
                    SciantixArray<InputVariable>&    scaling_factors)
{
    System system_;

    system_.setName("Xe in UO2HBS");
    system_.setGas(gas_fp["Xe"]);
    system_.setMatrix(matrices["UO2HBS"]);
    system_.setRestructuredMatrix(1);
    system_.setYield(0.24);
    system_.setRadiusInLattice(0.21e-9);
    system_.setVolumeInLattice(matrices["UO2HBS"].getSchottkyVolume());
    system_.setHenryConstant(0.0);
    system_.setProductionRate(5, history_variable, input_variable, sciantix_variable, scaling_factors);
    system_.setFissionProductDiffusivity(5, sciantix_variable, history_variable, scaling_factors, matrices);
    system_.setBubbleDiffusivity(0, sciantix_variable, history_variable, matrices);
    system_.setResolutionRate(99, sciantix_variable, history_variable, scaling_factors, matrices);
    system_.setTrappingRate(99, sciantix_variable, scaling_factors);
    system_.setNucleationRate(99, history_variable, scaling_factors);

    return system_;
}

System Kr_in_UO2(SciantixArray<Matrix>&           matrices,
                 SciantixArray<FissionProducts>&  gas_fp,
                 SciantixArray<InputVariable>&    input_variable,
                 SciantixArray<SciantixVariable>& sciantix_variable,
                 SciantixArray<SciantixVariable>& history_variable,
                 SciantixArray<InputVariable>&    scaling_factors)
{
    System system_;

    system_.setName("Kr in UO2");
    system_.setGas(gas_fp["Kr"]);
    system_.setMatrix(matrices["UO2"]);
    system_.setRestructuredMatrix(0);
    system_.setYield(0.03);
    system_.setRadiusInLattice(0.21e-9);
    system_.setVolumeInLattice(matrices["UO2"].getSchottkyVolume());
    system_.setHenryConstant(0.0);
    system_.setProductionRate(1, history_variable, input_variable, sciantix_variable, scaling_factors);
    system_.setFissionProductDiffusivity(int(input_variable["iFissionProductDiffusivity"].getValue()),
                                         sciantix_variable,
                                         history_variable,
                                         scaling_factors,
                                         matrices);
    system_.setBubbleDiffusivity(
        int(input_variable["iBubbleDiffusivity"].getValue()), sciantix_variable, history_variable, matrices);
    system_.setResolutionRate(int(input_variable["iResolutionRate"].getValue()),
                              sciantix_variable,
                              history_variable,
                              scaling_factors,
                              matrices);
    system_.setTrappingRate(int(input_variable["iTrappingRate"].getValue()), sciantix_variable, scaling_factors);
    system_.setNucleationRate(int(input_variable["iNucleationRate"].getValue()), history_variable, scaling_factors);

    return system_;
}

System He_in_UO2(SciantixArray<Matrix>&           matrices,
                 SciantixArray<FissionProducts>&  gas_fp,
                 SciantixArray<InputVariable>&    input_variable,
                 SciantixArray<SciantixVariable>& sciantix_variable,
                 SciantixArray<SciantixVariable>& history_variable,
                 SciantixArray<InputVariable>&    scaling_factors)
{
    System system_;

    system_.setName("He in UO2");
    system_.setGas(gas_fp["He"]);
    system_.setMatrix(matrices["UO2"]);
    system_.setHenryConstant(
        4.1e+18 * exp(-7543.5 / history_variable["Temperature"]
                                    .getFinalValue()));  /// The Henry's constant for helium in UO<sub>2</sub>-single
                                                         /// crystal samples is set from best estimate correlation after
                                                         /// @ref *L. Cognini et al. Nuclear Engineering and Design 340
                                                         /// (2018) 240–244*. This correlation is valid in the
                                                         /// temperature range 1073-1773 K.
    system_.setRestructuredMatrix(0);
    system_.setYield(0.0022);  // from ternary fissions
    system_.setRadiusInLattice(4.73e-11);
    system_.setVolumeInLattice(matrices["UO2"].getOctahedralInterstitialSite());
    system_.setHeliumDiffusivity(int(input_variable["iHeDiffusivity"].getValue()), history_variable);
    system_.setResolutionRate(int(input_variable["iResolutionRate"].getValue()),
                              sciantix_variable,
                              history_variable,
                              scaling_factors,
                              matrices);
    system_.setTrappingRate(int(input_variable["iTrappingRate"].getValue()), sciantix_variable, scaling_factors);
    system_.setNucleationRate(int(input_variable["iNucleationRate"].getValue()), history_variable, scaling_factors);
    system_.setProductionRate(int(input_variable["iHeliumProductionRate"].getValue()),
                              history_variable,
                              input_variable,
                              sciantix_variable,
                              scaling_factors);
    system_.setBubbleDiffusivity(
        int(input_variable["iBubbleDiffusivity"].getValue()), sciantix_variable, history_variable, matrices);

    return system_;
}

System Xe133_in_UO2(SciantixArray<Matrix>&           matrices,
                    SciantixArray<FissionProducts>&  gas_fp,
                    SciantixArray<InputVariable>&    input_variable,
                    SciantixArray<SciantixVariable>& sciantix_variable,
                    SciantixArray<SciantixVariable>& history_variable,
                    SciantixArray<InputVariable>&    scaling_factors)
{
    System system_;

    system_.setName("Xe133 in UO2");
    system_.setGas(gas_fp["Xe133"]);
    system_.setMatrix(matrices["UO2"]);
    system_.setRestructuredMatrix(0);
    system_.setYield(0.066534);           // from JEFF-3.3 library
    system_.setRadiusInLattice(0.21e-9);  // (m), number from experimental results, assumed equal for Xe and Kr
    system_.setVolumeInLattice(matrices["UO2"].getSchottkyVolume());
    system_.setHenryConstant(0.0);
    system_.setProductionRate(1, history_variable, input_variable, sciantix_variable, scaling_factors);
    system_.setFissionProductDiffusivity(int(input_variable["iFissionProductDiffusivity"].getValue()),
                                         sciantix_variable,
                                         history_variable,
                                         scaling_factors,
                                         matrices);
    system_.setBubbleDiffusivity(
        int(input_variable["iBubbleDiffusivity"].getValue()), sciantix_variable, history_variable, matrices);
    system_.setResolutionRate(int(input_variable["iResolutionRate"].getValue()),
                              sciantix_variable,
                              history_variable,
                              scaling_factors,
                              matrices);
    system_.setTrappingRate(int(input_variable["iTrappingRate"].getValue()), sciantix_variable, scaling_factors);
    system_.setNucleationRate(int(input_variable["iNucleationRate"].getValue()), history_variable, scaling_factors);

    return system_;
}

System Kr85m_in_UO2(SciantixArray<Matrix>&           matrices,
                    SciantixArray<FissionProducts>&  gas_fp,
                    SciantixArray<InputVariable>&    input_variable,
                    SciantixArray<SciantixVariable>& sciantix_variable,
                    SciantixArray<SciantixVariable>& history_variable,
                    SciantixArray<InputVariable>&    scaling_factors)
{
    System system_;

    system_.setName("Kr85m in UO2");
    system_.setGas(gas_fp["Kr85m"]);
    system_.setMatrix(matrices["UO2"]);
    system_.setRestructuredMatrix(0);
    system_.setYield(0.013027);
    system_.setRadiusInLattice(0.21e-9);
    system_.setVolumeInLattice(matrices["UO2"].getSchottkyVolume());
    system_.setHenryConstant(0.0);
    system_.setProductionRate(1, history_variable, input_variable, sciantix_variable, scaling_factors);
    system_.setFissionProductDiffusivity(int(input_variable["iFissionProductDiffusivity"].getValue()),
                                         sciantix_variable,
                                         history_variable,
                                         scaling_factors,
                                         matrices);
    system_.setBubbleDiffusivity(
        int(input_variable["iBubbleDiffusivity"].getValue()), sciantix_variable, history_variable, matrices);
    system_.setResolutionRate(int(input_variable["iResolutionRate"].getValue()),
                              sciantix_variable,
                              history_variable,
                              scaling_factors,
                              matrices);
    system_.setTrappingRate(int(input_variable["iTrappingRate"].getValue()), sciantix_variable, scaling_factors);
    system_.setNucleationRate(int(input_variable["iNucleationRate"].getValue()), history_variable, scaling_factors);

    return system_;
}

// MOX

System Xe_in_MOX(SciantixArray<Matrix>&           matrices,
                 SciantixArray<FissionProducts>&  gas_fp,
                 SciantixArray<InputVariable>&    input_variable,
                 SciantixArray<SciantixVariable>& sciantix_variable,
                 SciantixArray<SciantixVariable>& history_variable,
                 SciantixArray<InputVariable>&    scaling_factors)
{
    System system_;

    system_.setName("Xe in MOX");
    system_.setGas(gas_fp["Xe"]);
    system_.setMatrix(matrices["MOX"]);
    system_.setRestructuredMatrix(0);
    system_.setYield(0.234);
    system_.setRadiusInLattice(0.21e-9);  // (m), from experimental data, assumed equal for Xe and Kr, equal to UO2
    system_.setVolumeInLattice(matrices["MOX"].getSchottkyVolume());
    system_.setHenryConstant(0.0);  // Rest, 1992; Walker, 1977 for Xe and Kr typical value is 10^-7
    system_.setProductionRate(1,
                              history_variable,
                              input_variable,
                              sciantix_variable,
                              scaling_factors);  // Production rate as in UO₂ (Turnbull, 1988)
    system_.setFissionProductDiffusivity(int(input_variable["iFissionProductDiffusivity"].getValue()),
                                         sciantix_variable,
                                         history_variable,
                                         scaling_factors,
                                         matrices);  // Lanning (2005); OECD/NEA (2019) – higher diffusivity in MOX due
                                                     // to porosity and Pu content 1.75 * for MOX?
    system_.setBubbleDiffusivity(
        int(input_variable["iBubbleDiffusivity"].getValue()), sciantix_variable, history_variable, matrices);
    system_.setResolutionRate(int(input_variable["iResolutionRate"].getValue()),
                              sciantix_variable,
                              history_variable,
                              scaling_factors,
                              matrices);
    system_.setTrappingRate(int(input_variable["iTrappingRate"].getValue()), sciantix_variable, scaling_factors);
    system_.setNucleationRate(int(input_variable["iNucleationRate"].getValue()), history_variable, scaling_factors);

    return system_;
}

System Kr_in_MOX(SciantixArray<Matrix>&           matrices,
                 SciantixArray<FissionProducts>&  gas_fp,
                 SciantixArray<InputVariable>&    input_variable,
                 SciantixArray<SciantixVariable>& sciantix_variable,
                 SciantixArray<SciantixVariable>& history_variable,
                 SciantixArray<InputVariable>&    scaling_factors)
{
    System system_;

    system_.setName("Kr in MOX");
    system_.setGas(gas_fp["Kr"]);
    system_.setMatrix(matrices["MOX"]);
    system_.setRestructuredMatrix(0);
    system_.setYield(0.017);
    system_.setRadiusInLattice(0.21e-9);  // (m), from experimental data, assumed equal for Xe and Kr, equal to UO2
    system_.setVolumeInLattice(matrices["MOX"].getSchottkyVolume());
    system_.setHenryConstant(0.0);  // Rest, 1992; Walker, 1977 for Xe and Kr typical value is 10^-7
    system_.setProductionRate(
        1,
        history_variable,
        input_variable,
        sciantix_variable,
        scaling_factors);  // MOX: using same production model as UO2 (no dedicated MOX correlation)
    system_.setFissionProductDiffusivity(int(input_variable["iFissionProductDiffusivity"].getValue()),
                                         sciantix_variable,
                                         history_variable,
                                         scaling_factors,
                                         matrices);  // Lanning (2005); OECD/NEA (2019) – higher diffusivity in MOX due
                                                     // to porosity and Pu content 1.75 * for MOX?
    system_.setBubbleDiffusivity(
        int(input_variable["iBubbleDiffusivity"].getValue()), sciantix_variable, history_variable, matrices);
    system_.setResolutionRate(int(input_variable["iResolutionRate"].getValue()),
                              sciantix_variable,
                              history_variable,
                              scaling_factors,
                              matrices);
    system_.setTrappingRate(int(input_variable["iTrappingRate"].getValue()), sciantix_variable, scaling_factors);
    system_.setNucleationRate(int(input_variable["iNucleationRate"].getValue()), history_variable, scaling_factors);

    return system_;
}

System He_in_MOX(SciantixArray<Matrix>&           matrices,
                 SciantixArray<FissionProducts>&  gas_fp,
                 SciantixArray<InputVariable>&    input_variable,
                 SciantixArray<SciantixVariable>& sciantix_variable,
                 SciantixArray<SciantixVariable>& history_variable,
                 SciantixArray<InputVariable>&    scaling_factors)
{
    System system_;

    system_.setName("He in MOX");
    system_.setGas(gas_fp["He"]);
    system_.setMatrix(matrices["MOX"]);
    system_.setHenryConstant(
        4.1e+18 *
        exp(-7543.5 / history_variable["Temperature"]
                          .getFinalValue()));  // from Matzke, JNM 65 (1977) 89-106, correlation used for UO2 and MOX
    system_.setRestructuredMatrix(0);
    system_.setYield(0.008);
    system_.setRadiusInLattice(4.73e-11);  // (m), value of UO₂, valid also for MOX
    system_.setVolumeInLattice(matrices["MOX"].getOctahedralInterstitialSite());
    system_.setHeliumDiffusivity(int(input_variable["iHeDiffusivity"].getValue()), history_variable);
    system_.setResolutionRate(int(input_variable["iResolutionRate"].getValue()),
                              sciantix_variable,
                              history_variable,
                              scaling_factors,
                              matrices);
    system_.setTrappingRate(int(input_variable["iTrappingRate"].getValue()), sciantix_variable, scaling_factors);
    system_.setNucleationRate(int(input_variable["iNucleationRate"].getValue()), history_variable, scaling_factors);
    system_.setProductionRate(
        2,
        history_variable,
        input_variable,
        sciantix_variable,
        scaling_factors);  // A. Cechet et al., Nuclear Engineering and Technology (2021), case 2 in system.c
    system_.setBubbleDiffusivity(
        int(input_variable["iBubbleDiffusivity"].getValue()), sciantix_variable, history_variable, matrices);

    return system_;
}

System Xe133_in_MOX(SciantixArray<Matrix>&           matrices,
                    SciantixArray<FissionProducts>&  gas_fp,
                    SciantixArray<InputVariable>&    input_variable,
                    SciantixArray<SciantixVariable>& sciantix_variable,
                    SciantixArray<SciantixVariable>& history_variable,
                    SciantixArray<InputVariable>&    scaling_factors)
{
    System system_;

    system_.setName("Xe133 in MOX");
    system_.setGas(gas_fp["Xe133"]);
    system_.setMatrix(matrices["MOX"]);
    system_.setRestructuredMatrix(0);
    system_.setYield(0.066534);           // from JEFF-3.3 library
    system_.setRadiusInLattice(0.21e-9);  // (m), from experimental data, assumed equal for Xe and Kr, equal to UO2
    system_.setVolumeInLattice(matrices["MOX"].getSchottkyVolume());
    system_.setHenryConstant(0.0);  // Rest, 1992; Walker, 1977 for Xe and Kr typical value is 10^-7
    system_.setProductionRate(
        1,
        history_variable,
        input_variable,
        sciantix_variable,
        scaling_factors);  // MOX: using same production model as UO2 (no dedicated MOX correlation)
    system_.setFissionProductDiffusivity(
        int(input_variable["iFissionProductDiffusivity"].getValue()),
        sciantix_variable,
        history_variable,
        scaling_factors,
        matrices);  // Lanning (2005); OECD/NEA (2019) – higher diffusivity in MOX due to porosity and Pu content
    system_.setBubbleDiffusivity(
        int(input_variable["iBubbleDiffusivity"].getValue()), sciantix_variable, history_variable, matrices);
    system_.setResolutionRate(int(input_variable["iResolutionRate"].getValue()),
                              sciantix_variable,
                              history_variable,
                              scaling_factors,
                              matrices);
    system_.setTrappingRate(int(input_variable["iTrappingRate"].getValue()), sciantix_variable, scaling_factors);
    system_.setNucleationRate(int(input_variable["iNucleationRate"].getValue()), history_variable, scaling_factors);

    return system_;
}

System Kr85m_in_MOX(SciantixArray<Matrix>&           matrices,
                    SciantixArray<FissionProducts>&  gas_fp,
                    SciantixArray<InputVariable>&    input_variable,
                    SciantixArray<SciantixVariable>& sciantix_variable,
                    SciantixArray<SciantixVariable>& history_variable,
                    SciantixArray<InputVariable>&    scaling_factors)
{
    System system_;

    system_.setName("Kr85m in MOX");
    system_.setGas(gas_fp["Kr85m"]);
    system_.setMatrix(matrices["MOX"]);
    system_.setRestructuredMatrix(0);
    system_.setYield(0.013027);
    system_.setRadiusInLattice(0.21e-9);  // (m), from experimental data, assumed equal for Xe and Kr, equal to UO2
    system_.setVolumeInLattice(matrices["MOX"].getSchottkyVolume());
    system_.setHenryConstant(0.0);  // Rest, 1992; Walker, 1977 for Xe and Kr typical value is 10^-7
    system_.setProductionRate(
        1,
        history_variable,
        input_variable,
        sciantix_variable,
        scaling_factors);  // MOX: using same production model as UO2 (no dedicated MOX correlation)
    system_.setFissionProductDiffusivity(int(input_variable["iFissionProductDiffusivity"].getValue()),
                                         sciantix_variable,
                                         history_variable,
                                         scaling_factors,
                                         matrices);  // Lanning (2005); OECD/NEA (2019) – higher diffusivity in MOX due
                                                     // to porosity and Pu content 1.75 * for MOX?
    system_.setBubbleDiffusivity(
        int(input_variable["iBubbleDiffusivity"].getValue()), sciantix_variable, history_variable, matrices);
    system_.setResolutionRate(int(input_variable["iResolutionRate"].getValue()),
                              sciantix_variable,
                              history_variable,
                              scaling_factors,
                              matrices);
    system_.setTrappingRate(int(input_variable["iTrappingRate"].getValue()), sciantix_variable, scaling_factors);
    system_.setNucleationRate(int(input_variable["iNucleationRate"].getValue()), history_variable, scaling_factors);

    return system_;
}

System Cs_in_MOX(SciantixArray<Matrix>&           matrices,
                 SciantixArray<FissionProducts>&  volatile_fp,
                 SciantixArray<InputVariable>&    input_variable,
                 SciantixArray<SciantixVariable>& sciantix_variable,
                 SciantixArray<SciantixVariable>& history_variable,
                 SciantixArray<InputVariable>&    scaling_factors)
{
    System system_;

    system_.setName("Cs in MOX");
    system_.setVolatileFP(volatile_fp["Cs"]);
    system_.setMatrix(matrices["MOX"]);
    system_.setRestructuredMatrix(0);
    system_.setYield(0.170 *
                     scaling_factors["Cs production"]
                         .getValue());  // Cs from Samuelsson, K., Dumas, J. C., Sundman, B., Lamontagne, J., & Guéneau,
                                        // C. (2020). Simulation of the chemical state of high burnup (U,Pu)O2 fuel in
                                        // fast reactors based on thermodynamic calculations. Journal of Nuclear
                                        // Materials, 532(1), 151969. https://doi.org/10.1016/j.jnucmat.2019.151969)
    system_.setRadiusInLattice(0.21e-9);
    system_.setVolumeInLattice(matrices["MOX"].getSchottkyVolume());
    system_.setHenryConstant(0.0);
    system_.setProductionRate(
        1,
        history_variable,
        input_variable,
        sciantix_variable,
        scaling_factors);  // MOX: using same production model as UO2 (no dedicated MOX correlation)
    system_.setFissionProductDiffusivity(int(input_variable["iFissionProductDiffusivity"].getValue()),
                                         sciantix_variable,
                                         history_variable,
                                         scaling_factors,
                                         matrices);  // Lanning (2005); OECD/NEA (2019) – higher diffusivity in MOX due
                                                     // to porosity and Pu content 1.75 * for MOX?
    system_.setBubbleDiffusivity(
        int(input_variable["iBubbleDiffusivity"].getValue()), sciantix_variable, history_variable, matrices);
    system_.setResolutionRate(int(input_variable["iResolutionRate"].getValue()),
                              sciantix_variable,
                              history_variable,
                              scaling_factors,
                              matrices);
    system_.setTrappingRate(int(input_variable["iTrappingRate"].getValue()), sciantix_variable, scaling_factors);
    system_.setNucleationRate(int(input_variable["iNucleationRate"].getValue()), history_variable, scaling_factors);

    return system_;
}

// Metals in MOX
System Mo_in_MOX(SciantixArray<Matrix>&           matrices,
                 SciantixArray<FissionProducts>&  metallic_fp,
                 SciantixArray<InputVariable>&    input_variable,
                 SciantixArray<SciantixVariable>& sciantix_variable,
                 SciantixArray<SciantixVariable>& history_variable,
                 SciantixArray<InputVariable>&    scaling_factors)
{
    System system_;
    system_.setName("Mo in MOX");
    system_.setMetallicFP(metallic_fp["Mo"]);
    system_.setMatrix(matrices["MOX"]);
    system_.setRestructuredMatrix(0);
    system_.setYield(0.219);
    system_.setRadiusInLattice(0.15e-9);
    system_.setVolumeInLattice(matrices["MOX"].getSchottkyVolume());
    system_.setHenryConstant(0.0);
    system_.setProductionRate(1, history_variable, input_variable, sciantix_variable, scaling_factors);
    // [FMP, Nov. 2026]: metal diffusivity read by the FMP model, Turnbull et al. (1988) as for Xe
    system_.setFissionProductDiffusivity(1, sciantix_variable, history_variable, scaling_factors, matrices);
    return system_;
}

System Ba_in_MOX(SciantixArray<Matrix>&           matrices,
                 SciantixArray<FissionProducts>&  ceramic_fp,
                 SciantixArray<InputVariable>&    input_variable,
                 SciantixArray<SciantixVariable>& sciantix_variable,
                 SciantixArray<SciantixVariable>& history_variable,
                 SciantixArray<InputVariable>&    scaling_factors)
{
    System system_;
    system_.setName("Ba in MOX");
    system_.setCeramicFP(ceramic_fp["Ba"]);
    system_.setMatrix(matrices["MOX"]);
    system_.setRestructuredMatrix(0);
    system_.setYield(0.099);
    system_.setRadiusInLattice(0.215e-9);
    system_.setVolumeInLattice(matrices["MOX"].getSchottkyVolume());
    system_.setHenryConstant(0.0);
    system_.setProductionRate(1, history_variable, input_variable, sciantix_variable, scaling_factors);
    return system_;
}

System Ru_in_MOX(SciantixArray<Matrix>&           matrices,
                 SciantixArray<FissionProducts>&  metallic_fp,
                 SciantixArray<InputVariable>&    input_variable,
                 SciantixArray<SciantixVariable>& sciantix_variable,
                 SciantixArray<SciantixVariable>& history_variable,
                 SciantixArray<InputVariable>&    scaling_factors)
{
    System system_;
    system_.setName("Ru in MOX");
    system_.setMetallicFP(metallic_fp["Ru"]);
    system_.setMatrix(matrices["MOX"]);
    system_.setRestructuredMatrix(0);
    system_.setYield(0.198);
    system_.setRadiusInLattice(0.15e-9);
    system_.setVolumeInLattice(matrices["MOX"].getSchottkyVolume());
    system_.setHenryConstant(0.0);
    system_.setProductionRate(1, history_variable, input_variable, sciantix_variable, scaling_factors);
    return system_;
}

System Tc_in_MOX(SciantixArray<Matrix>&           matrices,
                 SciantixArray<FissionProducts>&  metallic_fp,
                 SciantixArray<InputVariable>&    input_variable,
                 SciantixArray<SciantixVariable>& sciantix_variable,
                 SciantixArray<SciantixVariable>& history_variable,
                 SciantixArray<InputVariable>&    scaling_factors)
{
    System system_;
    system_.setName("Tc in MOX");
    system_.setMetallicFP(metallic_fp["Tc"]);
    system_.setMatrix(matrices["MOX"]);
    system_.setRestructuredMatrix(0);
    system_.setYield(0.053);
    system_.setRadiusInLattice(0.15e-9);
    system_.setVolumeInLattice(matrices["MOX"].getSchottkyVolume());
    system_.setHenryConstant(0.0);
    system_.setProductionRate(1, history_variable, input_variable, sciantix_variable, scaling_factors);
    return system_;
}

System Pd_in_MOX(SciantixArray<Matrix>&           matrices,
                 SciantixArray<FissionProducts>&  metallic_fp,
                 SciantixArray<InputVariable>&    input_variable,
                 SciantixArray<SciantixVariable>& sciantix_variable,
                 SciantixArray<SciantixVariable>& history_variable,
                 SciantixArray<InputVariable>&    scaling_factors)
{
    System system_;
    system_.setName("Pd in MOX");
    system_.setMetallicFP(metallic_fp["Pd"]);
    system_.setMatrix(matrices["MOX"]);
    system_.setRestructuredMatrix(0);
    system_.setYield(0.189);
    system_.setRadiusInLattice(0.15e-9);
    system_.setVolumeInLattice(matrices["MOX"].getSchottkyVolume());
    system_.setHenryConstant(0.0);
    system_.setProductionRate(1, history_variable, input_variable, sciantix_variable, scaling_factors);
    return system_;
}

System Rh_in_MOX(SciantixArray<Matrix>&           matrices,
                 SciantixArray<FissionProducts>&  metallic_fp,
                 SciantixArray<InputVariable>&    input_variable,
                 SciantixArray<SciantixVariable>& sciantix_variable,
                 SciantixArray<SciantixVariable>& history_variable,
                 SciantixArray<InputVariable>&    scaling_factors)
{
    System system_;
    system_.setName("Rh in MOX");
    system_.setMetallicFP(metallic_fp["Rh"]);
    system_.setMatrix(matrices["MOX"]);
    system_.setRestructuredMatrix(0);
    system_.setYield(0.059);
    system_.setRadiusInLattice(0.15e-9);
    system_.setVolumeInLattice(matrices["MOX"].getSchottkyVolume());
    system_.setHenryConstant(0.0);
    system_.setProductionRate(1, history_variable, input_variable, sciantix_variable, scaling_factors);
    return system_;
}

// [FMP, Nov. 2026]: Mo, Tc, Ru, Rh, Pd in UO2, for the five metal particles (FMP) model.
// Yields for LWR UO2: ENDF/B-VIII.0 cumulative fission yields summed over the stable or long-lived end-of-chain
// isotopes (Mo 95, 96, 97, 98, 100; Tc 99; Ru 100, 101, 102, 104; Rh 103; Pd 104, 105, 106, 107, 108, 110),
// weighted with the cycle-average PWR fission fractions 235U 0.564 (thermal), 238U 0.076 (fast, 500 keV),
// 239Pu 0.304 and 241Pu 0.056 (thermal) of Daya Bay (D. Adey et al., Phys. Rev. Lett. 123 (2019) 111801).
// Sum 0.567 at/fiss (235U alone: 0.467). The MOX systems keep the fast-reactor yields. [TBC]
System MetallicFP_in_UO2(const std::string&               element,
                         SciantixArray<Matrix>&           matrices,
                         SciantixArray<FissionProducts>&  metallic_fp,
                         SciantixArray<InputVariable>&    input_variable,
                         SciantixArray<SciantixVariable>& sciantix_variable,
                         SciantixArray<SciantixVariable>& history_variable,
                         SciantixArray<InputVariable>&    scaling_factors)
{
    static const std::map<std::string, double> lwr_yield = {
        {"Mo", 0.2370}, {"Tc", 0.0614}, {"Ru", 0.1442}, {"Rh", 0.0469}, {"Pd", 0.0771}};

    System system_;
    system_.setName(element + " in UO2");
    system_.setMetallicFP(metallic_fp[element]);
    system_.setMatrix(matrices["UO2"]);
    system_.setRestructuredMatrix(0);
    system_.setYield(lwr_yield.at(element));
    system_.setRadiusInLattice(0.15e-9);
    system_.setVolumeInLattice(matrices["UO2"].getSchottkyVolume());
    system_.setHenryConstant(0.0);
    system_.setProductionRate(1, history_variable, input_variable, sciantix_variable, scaling_factors);
    // EC November 2026: metal diffusivity for the FMP growth, Turnbull et al. (1988) as for Xe (no Mo data)
    system_.setFissionProductDiffusivity(1, sciantix_variable, history_variable, scaling_factors, matrices);
    return system_;
}
