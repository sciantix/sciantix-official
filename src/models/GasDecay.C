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

#include "Simulation.h"

void Simulation::GasDecay()
{
    for (auto& system : sciantix_system)
    {
        if (gas[ga[system.getGasName()]].getDecayRate() > 0.0 && system.getRestructuredMatrix() == 0)
        {
            sciantix_variable[sv[system.getGasName() + " decayed"]].setFinalValue(
                solver.Decay(
                    sciantix_variable[sv[system.getGasName() + " decayed"]].getInitialValue(),
                    gas[ga[system.getGasName()]].getDecayRate(),
                    gas[ga[system.getGasName()]].getDecayRate() * sciantix_variable[sv[system.getGasName() + " produced"]].getFinalValue(),
                    physics_variable[pv["Time step"]].getFinalValue()
                )
            );
        }
    }
}