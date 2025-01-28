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
#include "SetFuel.h"

void Simulation::setFuel()
{
    switch (int(input_variable["iFuel"].getValue()))
    {
        case 0: 
        {
            fuels.push(monolithic_UO2(fuels, sciantix_variable, history_variable, input_variable));
            break;
        }

        case 1: 
        {
            fuels.push(monolithic_MOX(fuels, sciantix_variable, history_variable, input_variable));
            break;
        }
        
        case 2: 
        {
            matrices.push(TRISO_in_slab(fuels, sciantix_variable, history_variable, input_variable));
            break;
        }

        default:
            ErrorMessages::Switch(__FILE__, "iFuel", int(input_variable["iFuel"].getValue()));
            break;
    }
}

Fuel monolithic_UO2(SciantixArray<Fuel> &fuels, SciantixArray<SciantixVariable> &sciantix_variable, 
    SciantixArray<SciantixVariable> &history_variable, SciantixArray<InputVariable> &input_variable)
{
    Fuel fuel_;

    fuel_.setName("monolithic UO2");
    fuel_.setTheoreticalDensity(10960.0); // (kg/m3)

    return fuel_;
}

Fuel monolithic_MOX(SciantixArray<Fuel> &fuels, SciantixArray<SciantixVariable> &sciantix_variable, 
    SciantixArray<SciantixVariable> &history_variable, SciantixArray<InputVariable> &input_variable)
{
    Fuel fuel_;

    fuel_.setName("monolithic MOX");
    fuel_.setTheoreticalDensity(10960.0); // (kg/m3)

    return fuel_;
}

Fuel TRISO_in_slab(SciantixArray<Fuel> &fuels, SciantixArray<SciantixVariable> &sciantix_variable, 
    SciantixArray<SciantixVariable> &history_variable, SciantixArray<InputVariable> &input_variable)
{
    Fuel fuel_;

    fuel_.setName("TRISO in slab");
    fuel_.setTheoreticalDensity(10960.0); // (kg/m3)

    return fuel_;
}
