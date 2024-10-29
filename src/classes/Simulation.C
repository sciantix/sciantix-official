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
    double Sciantix_diffusion_modes[]
)
{
    setVariables(
        Sciantix_options,
        Sciantix_history,
        Sciantix_variables,
        Sciantix_scaling_factors,
        Sciantix_diffusion_modes
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
    #endif

    GapPartialPressure();

    UO2Thermochemistry();

    StoichiometryDeviation();

    HighBurnupStructureFormation();

    HighBurnupStructurePorosity();

    Microstructure();

    ChromiumSolubility();

    GrainGrowth();

    GrainBoundarySweeping();

    GasProduction();

    GasDecay();

    IntraGranularBubbleBehavior();

    GasDiffusion();

    GrainBoundaryMicroCracking();

    GrainBoundaryVenting();

    InterGranularBubbleBehavior();
}