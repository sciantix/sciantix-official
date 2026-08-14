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
//  Version: 2.2.1                                                                  //
//  Year: 2026                                                                      //
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "Simulation.h"
#include <chrono>
#include <iostream>

Simulation* Simulation::getInstance()
{
    static Simulation instance;
    return &instance;
}

void Simulation::initialize(int    Sciantix_options[],
                            double Sciantix_history[],
                            double Sciantix_variables[],
                            double Sciantix_scaling_factors[],
                            double Sciantix_diffusion_modes[])
{
    setVariables(
        Sciantix_options, Sciantix_history, Sciantix_variables, Sciantix_scaling_factors, Sciantix_diffusion_modes);
    setGas();
    setMatrix();
    setSystem();
    setGPVariables(Sciantix_options, Sciantix_history, Sciantix_variables);
}

void Simulation::execute()
{
#if !defined(COUPLING_TU)
    Burnup();

    EffectiveBurnup();

    Densification();
#endif

    // The HBS models straddle the gas-behaviour block: the restructured volume fraction is
    // needed before anything else consumes it, while the porosity model consumes the grain
    // boundary inventory that GasDiffusion produces in the same time step.
    HighBurnupStructureFormation();

    GapPartialPressure();

    UO2Thermochemistry();

    StoichiometryDeviation();

    Microstructure();

    ChromiumSolubility();

    GrainGrowth();

    GrainBoundarySweeping();

    GasProduction();

    GasDecay();

    IntraGranularBubbleBehavior();

    GasDiffusion();

    HighBurnupStructurePorosity();

    GrainBoundaryMicroCracking();

    GrainBoundaryVenting();

    InterGranularBubbleBehavior();

    GasRelease();
}