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

#include "Simulation.h"
#include <algorithm>
#include <chrono>
#include <iostream>

Simulation* Simulation::instance = nullptr;

Simulation* Simulation::getInstance()
{
    if (instance == nullptr)
    {
        instance = new Simulation;
    }
    return instance;
}

void Simulation::initialize(int    Sciantix_options[],
                            double Sciantix_history[],
                            double Sciantix_variables[],
                            double Sciantix_scaling_factors[],
                            double Sciantix_diffusion_modes[])
{
    un_l3_active = Sciantix_options[11] == 3;
    if (un_l3_active)
    {
        if (!un_l3_model)
            un_l3_model.reset(new un_l3::Model(Sciantix_history, Sciantix_variables, Sciantix_scaling_factors));
        std::copy(Sciantix_history, Sciantix_history + 20, un_l3_history);
        return;
    }
    setVariables(
        Sciantix_options, Sciantix_history, Sciantix_variables, Sciantix_scaling_factors, Sciantix_diffusion_modes);
    setGas();
    setMatrix();
    setSystem();
    setGPVariables(Sciantix_options, Sciantix_history, Sciantix_variables);
}

void Simulation::execute()
{
    if (un_l3_active)
    {
        un_l3_model->execute(solver, un_l3_history);
        return; // All legacy physics updates belong to the other paths.
    }
#if !defined(COUPLING_TU)
    Burnup();

    EffectiveBurnup();

    Densification();
#endif

    GapPartialPressure();

    // UN AD URANIUMNITRIDE
    if (input_variable["iFuelMatrix"].getValue() != 2)
        UO2Thermochemistry();

    StoichiometryDeviation();

    HighBurnupStructureFormation();

    HighBurnupStructurePorosity();

    if (input_variable["iFuelMatrix"].getValue() != 2)
        Microstructure();

    if (input_variable["iFuelMatrix"].getValue() != 2)
        ChromiumSolubility();
    // END UN AD URANIUMNITRIDE

    GrainGrowth();

    GrainBoundarySweeping();

    GasProduction();

    GasDecay();

    // UN AD URANIUMNITRIDE
    if (input_variable["iIntraGranularBubbleBehavior"].getValue() == 5)
    {
        GasDiffusion();
        IntraGranularBubbleBehavior();
    }
    else
    {
        IntraGranularBubbleBehavior();
        GasDiffusion();
    }
    // END UN AD URANIUMNITRIDE

    GrainBoundaryMicroCracking();

    GrainBoundaryVenting();

    // UN AD URANIUMNITRIDE
    if (input_variable["iIntraGranularBubbleBehavior"].getValue() != 5)
    {
        InterGranularBubbleBehavior();
        GasRelease();
    }
    // END UN AD URANIUMNITRIDE
}
