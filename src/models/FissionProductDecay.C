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

void Simulation::FissionProductDecay()
{
    // Model declaration
    for (auto& system : sciantix_system) // qui tutti i FPs
    {
        if (system.getFissionProduct().getDecayRate() > 0.0 && system.getRestructuredMatrix() == 0)
        {
            sciantix_variable[system.getFissionProductName() + " decayed"].setFinalValue(solver.Decay(
                sciantix_variable[system.getFissionProductName() + " decayed"].getInitialValue(),
                system.getFissionProduct().getDecayRate(),
                system.getFissionProduct().getDecayRate() * sciantix_variable[system.getFissionProductName() + " produced"].getFinalValue(),
                physics_variable["Time step"].getFinalValue()));
        }
    }
}