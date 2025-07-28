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

#include "Simulation.h"
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

void Simulation::initialize(
    int Sciantix_options[],
    double Sciantix_history[],
    double Sciantix_variables[],
    double Sciantix_scaling_factors[],
    double Sciantix_diffusion_modes[],
    double Sciantix_thermochemistry[],
    std::vector<std::vector<std::string>> Sciantix_thermochemistry_options
)
{
    setVariables(
        Sciantix_options,
        Sciantix_history,
        Sciantix_variables,
        Sciantix_scaling_factors,
        Sciantix_diffusion_modes,
        Sciantix_thermochemistry,
        Sciantix_thermochemistry_options
    );
    setGas();
    setMatrix();
    setSystem();
}


void Simulation::execute()
{
    #if !defined(COUPLING_TU)
        Burnup();

        EffectiveBurnup();

        Densification();
    #endif

    GapPartialPressure();

    UO2Thermochemistry();

    StoichiometryDeviation();

    HighBurnupStructureFormation();

    HighBurnupStructurePorosity();

    Microstructure();

    ChromiumSolubility();

    GrainGrowth();

    GrainVaporisation();

    GrainBoundarySweeping();

    GasProduction();

    GasDecay();

    SetPhaseDiagram("in grain");

    IntraGranularBubbleBehavior();

    GasDiffusion();

    SetPhaseDiagram("at grain boundary");

    GrainBoundaryMicroCracking();

    GrainBoundaryVenting();

    InterGranularBubbleBehavior();

    GasRelease();
}