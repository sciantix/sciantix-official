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

#include "Sciantix.h"

void Sciantix(int    Sciantix_options[],
              double Sciantix_history[],
              double Sciantix_variables[],
              double Sciantix_scaling_factors[],
              double Sciantix_diffusion_modes[],
             // CODE DEVELOPMENT : THERMOCHEMISTRY VARIABLES/OPTIONS
              double Sciantix_thermochemistry[], 
              const ThermochemistrySettings& Sciantix_thermochemistry_options)
{
    Simulation* simulation = Simulation::getInstance();

    // CODE DEVELOPMENT : THERMOCHEMISTRY VARIABLES/OPTIONS
    simulation->initialize(Sciantix_options, Sciantix_history, Sciantix_variables, Sciantix_scaling_factors, Sciantix_diffusion_modes, Sciantix_thermochemistry, Sciantix_thermochemistry_options);
    //

    simulation->execute();

    // CODE DEVELOPMENT : THERMOCHEMISTRY UPDATE
    simulation->update(Sciantix_variables, Sciantix_diffusion_modes, Sciantix_thermochemistry);
    //
    
    simulation->output();
}