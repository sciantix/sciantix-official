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

void Simulation::GasDecay()
{
    // Model declaration
    for (auto& system : sciantix_system)
    {
        if (system.getGas().getDecayRate() > 0.0 && system.getRestructuredMatrix() == 0)
        {
            sciantix_variable[system.getGasName() + " decayed"].setFinalValue(solver.Decay(
                sciantix_variable[system.getGasName() + " decayed"].getInitialValue(),
                system.getGas().getDecayRate(),
                system.getGas().getDecayRate() * sciantix_variable[system.getGasName() + " produced"].getFinalValue(),
                physics_variable["Time step"].getFinalValue()));
        }
    }
}