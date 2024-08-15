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

#include "Sciantix.h"

void Sciantix(int Sciantix_options[],
    double Sciantix_history[],
    double Sciantix_variables[],
    double Sciantix_scaling_factors[],
    double Sciantix_diffusion_modes[])
{
    SetVariables(Sciantix_options, Sciantix_history, Sciantix_variables, Sciantix_scaling_factors, Sciantix_diffusion_modes);

    SetGas();

    SetMatrix();

    SetSystem();

    Simulation sciantix_simulation;

    sciantix_simulation.Burnup();

    sciantix_simulation.EffectiveBurnup();

    sciantix_simulation.GapPartialPressure();

    sciantix_simulation.UO2Thermochemistry();

    sciantix_simulation.StoichiometryDeviation(); 

    sciantix_simulation.HighBurnupStructureFormation();

    sciantix_simulation.HighBurnupStructurePorosity();

    sciantix_simulation.GrainGrowth();

    sciantix_simulation.GrainBoundarySweeping();

    sciantix_simulation.GasProduction();

    sciantix_simulation.GasDecay();

    sciantix_simulation.IntraGranularBubbleBehavior();

    sciantix_simulation.GasDiffusion();

    sciantix_simulation.GrainBoundaryMicroCracking();

    sciantix_simulation.GrainBoundaryVenting();

    sciantix_simulation.InterGranularBubbleBehavior();

    UpdateVariables(Sciantix_variables, Sciantix_diffusion_modes);

    Output();

    history_variable.clear();
    sciantix_variable.clear();
    sciantix_system.clear();
    physics_variable.clear();
    model.clear();
    material.clear();
    gas.clear();
    matrix.clear();
}
