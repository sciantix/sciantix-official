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
//  Version: 2.2.1                                                                    //
//  Year: 2025                                                                      //
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "SetSystem.h"
#include "Simulation.h"
#include "UNModel.h"
#include <cmath>

void Simulation::setSystem()
{
    switch ((int)input_variable["iFuelMatrix"].getValue())
    {
        case 0:
            sciantix_system.push(
                Xe_in_UO2(matrices, gas, input_variable, sciantix_variable, history_variable, scaling_factors));
            sciantix_system.push(
                Kr_in_UO2(matrices, gas, input_variable, sciantix_variable, history_variable, scaling_factors));
            sciantix_system.push(
                He_in_UO2(matrices, gas, input_variable, sciantix_variable, history_variable, scaling_factors));
            sciantix_system.push(
                Xe133_in_UO2(matrices, gas, input_variable, sciantix_variable, history_variable, scaling_factors));
            sciantix_system.push(
                Kr85m_in_UO2(matrices, gas, input_variable, sciantix_variable, history_variable, scaling_factors));
            break;

        case 1:
            sciantix_system.push(
                Xe_in_UO2(matrices, gas, input_variable, sciantix_variable, history_variable, scaling_factors));
            sciantix_system.push(
                Xe_in_UO2HBS(matrices, gas, input_variable, sciantix_variable, history_variable, scaling_factors));
            break;

            // UN AD URANIUMNITRIDE
        case 2:  // UN
            sciantix_system.push(
                Xe_in_UN(matrices, gas, input_variable, sciantix_variable, history_variable, scaling_factors));
            break;
            // END UN AD URANIUMNITRIDE

        default:
            break;
    }
}

System Xe_in_UO2(SciantixArray<Matrix>&           matrices,
                 SciantixArray<Gas>&              gas,
                 SciantixArray<InputVariable>&    input_variable,
                 SciantixArray<SciantixVariable>& sciantix_variable,
                 SciantixArray<SciantixVariable>& history_variable,
                 SciantixArray<InputVariable>&    scaling_factors)
{
    System system_;

    system_.setName("Xe in UO2");
    system_.setGas(gas["Xe"]);
    system_.setMatrix(matrices["UO2"]);
    system_.setRestructuredMatrix(0);
    system_.setYield(0.24);
    system_.setRadiusInLattice(0.21e-9);  // (m), from experimental data, assumed equal for Xe and Kr
    system_.setVolumeInLattice(matrices["UO2"].getSchottkyVolume());
    system_.setHenryConstant(0.0);
    system_.setProductionRate(1, history_variable, input_variable, sciantix_variable, scaling_factors);
    system_.setFissionGasDiffusivity(
        int(input_variable["iFissionGasDiffusivity"].getValue()), sciantix_variable, history_variable, scaling_factors);
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
                    SciantixArray<Gas>&              gas,
                    SciantixArray<InputVariable>&    input_variable,
                    SciantixArray<SciantixVariable>& sciantix_variable,
                    SciantixArray<SciantixVariable>& history_variable,
                    SciantixArray<InputVariable>&    scaling_factors)
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

System Kr_in_UO2(SciantixArray<Matrix>&           matrices,
                 SciantixArray<Gas>&              gas,
                 SciantixArray<InputVariable>&    input_variable,
                 SciantixArray<SciantixVariable>& sciantix_variable,
                 SciantixArray<SciantixVariable>& history_variable,
                 SciantixArray<InputVariable>&    scaling_factors)
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
    system_.setFissionGasDiffusivity(
        int(input_variable["iFissionGasDiffusivity"].getValue()), sciantix_variable, history_variable, scaling_factors);
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
                 SciantixArray<Gas>&              gas,
                 SciantixArray<InputVariable>&    input_variable,
                 SciantixArray<SciantixVariable>& sciantix_variable,
                 SciantixArray<SciantixVariable>& history_variable,
                 SciantixArray<InputVariable>&    scaling_factors)
{
    System system_;

    system_.setName("He in UO2");
    system_.setGas(gas["He"]);
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
                    SciantixArray<Gas>&              gas,
                    SciantixArray<InputVariable>&    input_variable,
                    SciantixArray<SciantixVariable>& sciantix_variable,
                    SciantixArray<SciantixVariable>& history_variable,
                    SciantixArray<InputVariable>&    scaling_factors)
{
    System system_;

    system_.setName("Xe133 in UO2");
    system_.setGas(gas["Xe133"]);
    system_.setMatrix(matrices["UO2"]);
    system_.setRestructuredMatrix(0);
    system_.setYield(0.066534);           // from JEFF-3.3 library
    system_.setRadiusInLattice(0.21e-9);  // (m), number from experimental results, assumed equal for Xe and Kr
    system_.setVolumeInLattice(matrices["UO2"].getSchottkyVolume());
    system_.setHenryConstant(0.0);
    system_.setProductionRate(1, history_variable, input_variable, sciantix_variable, scaling_factors);
    system_.setFissionGasDiffusivity(
        int(input_variable["iFissionGasDiffusivity"].getValue()), sciantix_variable, history_variable, scaling_factors);
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
                    SciantixArray<Gas>&              gas,
                    SciantixArray<InputVariable>&    input_variable,
                    SciantixArray<SciantixVariable>& sciantix_variable,
                    SciantixArray<SciantixVariable>& history_variable,
                    SciantixArray<InputVariable>&    scaling_factors)
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
    system_.setFissionGasDiffusivity(
        int(input_variable["iFissionGasDiffusivity"].getValue()), sciantix_variable, history_variable, scaling_factors);
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

// UN AD URANIUMNITRIDE
System Xe_in_UN(SciantixArray<Matrix>&           matrices,
                SciantixArray<Gas>&              gas,
                SciantixArray<InputVariable>&    input_variable,
                SciantixArray<SciantixVariable>& sciantix_variable,
                SciantixArray<SciantixVariable>& history_variable,
                SciantixArray<InputVariable>&    scaling_factors)
{
    System system_;

    system_.setName("Xe in UN");
    system_.setGas(gas["Xe"]);
    system_.setMatrix(matrices["UN"]);
    system_.setRestructuredMatrix(0);

    system_.setYield(0.24);  // TUTTI I PARAMETRI SONO INVENTATI
    system_.setRadiusInLattice(0.21e-9);

    system_.setVolumeInLattice(matrices["UN"].getSchottkyVolume());

    system_.setHenryConstant(0.0);
    system_.setBulkNucleationFactor(
        1.0e-6);  // sarebbe fn nella formula (26) Ritzk (suggerito 10^-6 ma range 10^-2  10^-7)

    system_.setProductionRate(1, history_variable, input_variable, sciantix_variable, scaling_factors);

    system_.setFissionGasDiffusivity(
        int(input_variable["iFissionGasDiffusivity"].getValue()), sciantix_variable, history_variable, scaling_factors);

    system_.setBubbleDiffusivity(
        int(input_variable["iBubbleDiffusivity"].getValue()), sciantix_variable, history_variable, matrices);

    system_.setResolutionRatesUN(int(input_variable["iResolutionRate"].getValue()),
                                 sciantix_variable,
                                 history_variable,
                                 scaling_factors,
                                 matrices);

    if (sciantix_variable["Dislocation density"].getFinalValue() <= 0.0)
    {
        const double lattice_parameter   = 4.889e-10;
        const double uranium_density     = 4.0 / std::pow(lattice_parameter, 3.0);
        const double fission_rate        = history_variable["Fission rate"].getFinalValue();
        const double time_s              = history_variable["Time"].getFinalValue() * 3600.0;
        const double burnup_percent_fima = 100.0 * fission_rate * time_s / uranium_density;
        const double rho_d =
            un_model::dynamic_dislocation_density(history_variable["Temperature"].getFinalValue(),
                                                  burnup_percent_fima,
                                                  int(input_variable["iUNDislocationDensity"].getValue()),
                                                  matrices["UN"].getDislocationDensity());
        sciantix_variable["Dislocation density"].setFinalValue(rho_d);
    }

    system_.setTrappingRatesUN(int(input_variable["iTrappingRate"].getValue()), sciantix_variable, scaling_factors);
    // per UN Ritzk nucleation rate non puo essere chiamato qui perche dipende da c^2 concentrazione gas in soluzione
    system_.setNucleationRate(int(input_variable["iNucleationRate"].getValue()), history_variable, scaling_factors);

    return system_;
}
// END UN AD URANIUMNITRIDE

System Kr_in_UN(SciantixArray<Matrix>&           matrices,
                SciantixArray<Gas>&              gas,
                SciantixArray<InputVariable>&    input_variable,
                SciantixArray<SciantixVariable>& sciantix_variable,
                SciantixArray<SciantixVariable>& history_variable,
                SciantixArray<InputVariable>&    scaling_factors)
{
    System system_;

    system_.setName("Kr in UN");
    system_.setGas(gas["Kr"]);
    system_.setMatrix(matrices["UN"]);
    system_.setRestructuredMatrix(0);

    system_.setYield(0.3);  // TUTTI I PARAMETRI SONO INVENTATI
    system_.setRadiusInLattice(0.21e-9);

    system_.setVolumeInLattice(matrices["UN"].getSchottkyVolume());

    system_.setHenryConstant(0.0);
    system_.setBulkNucleationFactor(1.0e-6);

    system_.setProductionRate(1, history_variable, input_variable, sciantix_variable, scaling_factors);

    system_.setFissionGasDiffusivity(
        int(input_variable["iFissionGasDiffusivity"].getValue()), sciantix_variable, history_variable, scaling_factors);

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
// per He solo formale, parametri copiati da UO2
System He_in_UN(SciantixArray<Matrix>&           matrices,
                SciantixArray<Gas>&              gas,
                SciantixArray<InputVariable>&    input_variable,
                SciantixArray<SciantixVariable>& sciantix_variable,
                SciantixArray<SciantixVariable>& history_variable,
                SciantixArray<InputVariable>&    scaling_factors)
{
    System system_;

    system_.setName("He in UN");
    system_.setGas(gas["He"]);

    // MATRICE UN
    system_.setMatrix(matrices["UN"]);
    system_.setRestructuredMatrix(0);

    // Henry NON noto per UN → metti 0 per ora
    system_.setHenryConstant(0.0);
    system_.setBulkNucleationFactor(1.0e-6);

    // Produzione He (ternary fission → ok uguale)
    system_.setYield(0.0022);

    // Raggio He (ok lascia uguale)
    system_.setRadiusInLattice(4.73e-11);

    // volume sito → usa quello della matrice UN
    system_.setVolumeInLattice(matrices["UN"].getOctahedralInterstitialSite());

    // Diffusività He (usa modello selezionato)
    system_.setHeliumDiffusivity(int(input_variable["iHeDiffusivity"].getValue()), history_variable);

    // Risoluzione bolle
    system_.setResolutionRate(int(input_variable["iResolutionRate"].getValue()),
                              sciantix_variable,
                              history_variable,
                              scaling_factors,
                              matrices);

    // Trapping
    system_.setTrappingRate(int(input_variable["iTrappingRate"].getValue()), sciantix_variable, scaling_factors);

    // Nucleazione
    system_.setNucleationRate(int(input_variable["iNucleationRate"].getValue()), history_variable, scaling_factors);

    // Produzione He
    system_.setProductionRate(int(input_variable["iHeliumProductionRate"].getValue()),
                              history_variable,
                              input_variable,
                              sciantix_variable,
                              scaling_factors);

    // Diffusività bolle
    system_.setBubbleDiffusivity(
        int(input_variable["iBubbleDiffusivity"].getValue()), sciantix_variable, history_variable, matrices);

    return system_;
}
